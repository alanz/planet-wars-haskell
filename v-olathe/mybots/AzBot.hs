{-# OPTIONS_GHC -O2 -fexcess-precision -fvia-C -optc-O3 #-}

-- Based on MyBot from Olafe, as a skeleton

import PlanetWars
import Data.Ord       (comparing)
import Data.List      (partition, minimumBy, maximumBy,sortBy,foldl')
import Control.Monad  (forever, when, unless)
import Debug.Trace
import qualified Data.Map as Map

-- ---------------------------------------------------------------------
{-
Strategy

1. Choose an appropriate target

1.a make some sort of impact measure, of potential gain, force needed, and time to achieve.

2. Fire fleets from all available planets

3. Use only the requisite force: send just enough to take the planet,
   bearing in mind the amount sent from the baddies,
   and own fleets in flight
   and growth rate x time of arrival

-  Do not target a planet if it is already being targeted with sufficient force

-. Current targeting sometimes chooses a planet already in my hands?

-. Look at defending my own planets that are under attack
   - Perhaps include planets that will change hands in future, based on en-route fleets

-. Currently, does not attack a target if there is no chance of victory in one salvo.
   Need to look at ganging up the attcks. http://72.44.46.68/canvas?game_id=141421

- Sometimes get stuck in no-hope strategy, need to bring an element of
  randomness into the play. http://72.44.46.68/canvas?game_id=154010

-}
{-
TODO:
  Understand this game http://72.44.46.68/canvas?game_id=153948
  against RageBot, only way to win is to first hit as hard as they do, or to do nothing. I think.
-}

fAzBot :: BotFunction  -- [Planet] -> [Fleet] -> [Fleet]
fAzBot planets fleets = 
  let 
    (myPlanets, notMyPlanets) = partition isMine planets
    enemyPlanets    = enemy notMyPlanets
    myProduction    = sum (map production myPlanets)
    enemyProduction = sum (map production enemyPlanets)
                      
    (myFleets,  enemyFleets)  = partition isMine fleets
    myShips         = sum (map ships myPlanets)    + sum (map ships myFleets)
    enemyShips      = sum (map ships enemyPlanets) + sum (map ships enemyFleets)
                              
    --candidates = notMyPlanets                      
                      
    fp = futurePlanets planets fleets                 
                 
    candidates = {- filter (\(p,_,_) -> not (isMine p)) $ -} Map.elems fp
                 
    source = maximumBy (comparing score) myPlanets
    sources = filter (\planet -> ships planet > 5) myPlanets
    
    sc2_candidates = concatMap (\src -> targetsForSource src candidates) myPlanets
    
    --debugStr = show(target)
    debugStr = ("#" ++ show(sc2_candidates))
                          
  in 
     if 
       -- trace(debugStr) True || -- Must comment out this line when playing the TCP server
       null myPlanets || null notMyPlanets || null sc2_candidates -- || (not . null . drop maxFleetsM1 $ myFleets)
       then []
       else 
         --[newFleet source target (div (ships source) 2)]
         map (\(_,(src,dst,_)) -> newFleet src dst ((ships dst) + 1)) sc2_candidates

-- ---------------------------------------------------------------------

targetsForSource src candidates = 
  let
    sc2_candidates' = sortBy rank 
                      $ map (\(dst,cntMine,cntEnemy) -> (src,dst,score2 src dst cntMine cntEnemy)) candidates
   
    cumulativeShips = tail $ scanl (+) 0 $ map (\(src,dst,_) -> 1 + (ships dst)) sc2_candidates'
    
    sc2_candidates = takeWhile (\(c,_) -> c < (ships src) - 5) $ zip cumulativeShips sc2_candidates'
  
    
    rank (_,_,a) (_,_,b) = compare b a -- descending order of score

  in
   sc2_candidates

-- ---------------------------------------------------------------------

score :: Planet -> Double
score p = fromIntegral (ships p)/(1 + fromIntegral (production p))

-- ---------------------------------------------------------------------

-- Higher score means a better/more pressing target
score2 :: Planet -> Planet -> ShipCount -> ShipCount -> Double
score2 src dst cntMine cntEnemy 
  | isMine dst  = scoreMine
  | isEnemy dst = scoreEnemy                 
  | otherwise   = scoreNeutral                  
  where
    shipsDstMine    = cntMine - cntEnemy + (ships dst)
    scoreMine = if (shipsDstMine > 5) then (0.0) else (scoreVal 1.0 (score dst))
    
    shipsDstEnemy   = cntMine - cntEnemy - (ships dst)
    
    shipsDstNeutral = cntMine - cntEnemy + (ships dst) -- TODO : proper calc, largest - sndlargest, toss 3rd
    
    dist = fromIntegral (distance src dst)
    pSuccess shipsDst = 
      if (ships src > shipsDst) 
        then (1.0) 
        else (1.0 * ( (fromIntegral (ships src - 5)) / (fromIntegral (shipsDst))))

    scoreVal ps s = (ps / s) / (1.5^(dist))

    scoreEnemy   = scoreVal (pSuccess shipsDstEnemy)   (score dst)
    scoreNeutral = scoreVal (pSuccess shipsDstNeutral) (score dst)
  
-- ---------------------------------------------------------------------

futurePlanets :: [Planet] -> [Fleet] -> Map.Map Planet (Planet, ShipCount, ShipCount)
futurePlanets planets fleets =
  let
    start = foldl' (\m p -> Map.insert (p) (p,0,0) m) Map.empty planets
    
    res = foldl' (\m f -> updateMap m f) start fleets
  in
    res

-- ---------------------------------------------------------------------
{-
-- TODO: need to take into account which fleets are going to which planets.
         Maybe keep a count of current as (cnt,owner), and then add tuples for
         each of mine,enemy arrivals
-}
-- updateMap
--   :: Map.Map Planet (Planet, ShipCount)
--      -> Fleet
--      -> Map.Map Planet (Planet, ShipCount)
updateMap m f =
  let
    tgt = target f
    (p,cntMine,cntEnemy) = m Map.! tgt
    new = if (isMine f) 
          then (p,cntMine + (ships f),cntEnemy)
          else (p,cntMine,cntEnemy + (ships f))               
  in
   Map.insert p new m

-- ---------------------------------------------------------------------

main = playAs fAzBot

-- EOF

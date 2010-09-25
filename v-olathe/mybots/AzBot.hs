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

- For an enemy planet, make allowance for production according to time of flight

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
       --trace(debugStr) True || -- Must comment out this line when playing the TCP server
       null myPlanets || null notMyPlanets || null sc2_candidates -- || (not . null . drop maxFleetsM1 $ myFleets)
       then []
       else 
         --[newFleet source target (div (ships source) 2)]
         map (\(_,(src,dst,(_,shipsDst))) -> newFleet src dst (shipsDst + 1)) sc2_candidates

-- ---------------------------------------------------------------------

targetsForSource src candidates = 
  let
    sc2_candidates' = sortBy rank 
                      $ filter (\(_,_,(_,shipsDst)) -> shipsDst > 0)
                      $ map (\(dst,cntMine,cntEnemy) -> (src,dst,score2 src dst cntMine cntEnemy)) candidates
   
    cumulativeShips = tail $ scanl (+) 0 $ map (\(_,_,(_,shipsDst)) -> 1 + shipsDst) sc2_candidates'
    
    sc2_candidates = takeWhile (\(c,_) -> c < (ships src) - 5) $ zip cumulativeShips sc2_candidates'
  
    
    rank (_,_,(a,_)) (_,_,(b,_)) = compare b a -- descending order of score

  in
   sc2_candidates

-- ---------------------------------------------------------------------

score :: Planet -> Double
score p = fromIntegral (ships p)/(1 + fromIntegral (production p))

-- ---------------------------------------------------------------------

-- Higher score means a better/more pressing target
score2 :: Planet -> Planet -> ShipCount -> ShipCount -> (Double, ShipCount)
score2 src dst cntMine cntEnemy 
  | isMine dst  = (scoreMine, shipsDstMine)
  | isEnemy dst = (scoreEnemy, shipsDstEnemy)
  | otherwise   = (scoreNeutral, shipsDstNeutral)
  where
    shipsDstMine    = cntMine - cntEnemy + (ships dst)
    scoreMine = if (shipsDstMine > 5) then (0.0) else (scoreVal 1.0 (score dst))
    
    shipsDstEnemy   = cntMine - cntEnemy - (ships dst) + (dist * (production dst))
    
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

-- ---------------------------------------------------------------------

test =
  let
    (planets,fleets) = head $ parseGameState $ concat mapStr                      
  in
   fAzBot planets fleets
   
mapStr = 
  [
  "P 11.152765 11.261845 0 119 1\n",
  "P 19.323053 19.415083 1 45 5\n",
  "P 2.982476 3.108607 2 58 5\n",
  "P 17.544563 22.212924 0 45 1\n",
  "P 4.760966 0.310766 0 45 1\n",
  "P 10.084655 4.773044 2 8 5\n",
  "P 12.220874 17.750646 1 1 5\n",
  "P 21.406539 17.735427 0 86 1\n",
  "P 0.898990 4.788263 0 86 1\n",
  "P 0.000000 2.053054 0 59 1\n",
  "P 22.305529 20.470636 0 59 1\n",
  "P 3.808495 5.491990 0 69 5\n",
  "P 18.497034 17.031701 1 38 5\n",
  "P 8.841685 12.712220 0 57 4\n",
  "P 13.463845 9.811470 0 57 4\n",
  "P 13.635701 7.877295 0 17 5\n",
  "P 8.669829 14.646395 0 17 5\n",
  "P 6.688247 18.076397 0 66 5\n",
  "P 15.617282 4.447293 0 66 5\n",
  "P 16.918396 0.696320 0 70 5\n",
  "P 5.387133 21.827370 0 70 5\n",
  "P 13.198469 0.000000 0 79 2\n",
  "P 9.107060 22.523690 0 79 2\n",
  "F 2 22 2 15 12 3\n",
  "F 2 1 2 20 19 11\n",
  "F 2 18 2 16 13 5\n",
  "F 2 3 2 16 13 6\n",
  "F 2 4 2 16 13 7\n",
  "F 2 1 2 5 8 3\n",
  "F 2 4 2 16 13 9\n",
  "F 2 4 2 16 13 10\n"
  ]
  
-- EOF

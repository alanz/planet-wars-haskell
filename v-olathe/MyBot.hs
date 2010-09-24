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

-. Currently, does not attack a target if there is no chance of victory in one salvo.
   Need to look at ganging up the attcks. http://72.44.46.68/canvas?game_id=141421

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
                 
    candidates = fp
                 
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
    sc2_candidates' = sortBy rank $ map (\(dst,cnt) -> (src,dst,score2 src dst cnt)) $ Map.elems candidates
   
    cumulativeShips = tail $ scanl (+) 0 $ map (\(src,dst,_) -> 1 + (ships dst)) sc2_candidates'
    
    sc2_candidates = takeWhile (\(c,_) -> c < (ships src) - 5) $ zip cumulativeShips sc2_candidates'
  
    
    rank (_,_,a) (_,_,b) = compare b a -- descending order of score

  in
   sc2_candidates

-- ---------------------------------------------------------------------

score :: Planet -> Double
score p = fromIntegral (ships p)/(1 + fromIntegral (production p))

-- ---------------------------------------------------------------------

score2 :: Planet -> Planet -> ShipCount -> Double
score2 src dst cnt = 
  let
    --shipsDst = ships dst
    shipsDst = cnt
    dist = fromIntegral (distance src dst)
    pSuccess = if (ships src > shipsDst) then (1.0) else (1.0 * ( (fromIntegral (ships src - 5)) / (fromIntegral (shipsDst))))
  in
   --pSuccess * fromIntegral (score dst) * (1.5^(-dist))
   (pSuccess / (score dst)) / (1.5^(dist))
  
-- ---------------------------------------------------------------------

futurePlanets :: [Planet] -> [Fleet] -> Map.Map Planet (Planet, ShipCount)
futurePlanets planets fleets =
  let
    static = map (\p -> (p,ships p)) planets
    --res = foldl' (\acc f -> )  static fleets
    
    start = foldl' (\m p -> Map.insert (p) (p,ships p) m) Map.empty planets
    
    res = foldl' (\m f -> updateMap m f) start fleets
  in
    res

-- ---------------------------------------------------------------------

updateMap m f =
  let
    tgt = target f
    (p,cnt) = m Map.! tgt
    new = (p, cnt + (ships f))
  in
   Map.insert p new m

-- ---------------------------------------------------------------------

main = playAs fAzBot

-- EOF

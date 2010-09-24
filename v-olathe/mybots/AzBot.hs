{-# OPTIONS_GHC -O2 -fexcess-precision -fvia-C -optc-O3 #-}

-- Based on MyBot from Olafe, as a skeleton

import PlanetWars
import Data.Ord       (comparing)
import Data.List      (partition, minimumBy, maximumBy,sortBy)
import Control.Monad  (forever, when, unless)
import Debug.Trace

-- ---------------------------------------------------------------------
{-
Strategy

1. Choose an appropriate target

1.a make some sort of impact measure, of potential gain, force needed, and time to achieve.

2. Fire fleets from all available planets

3. Use only the requisite force: send just enough to take the planet,
   bearing in mind the amount sent from the baddies,
   and growth rate x time of arrival

-  Do not target a planet if it is already being targeted with sufficient force

-. Current targeting sometimes chooses a planet already in my hands?

-. Look at defending my own planets that are under attack

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
                              
    (maxFleetsM1, candidates) = if myShips > enemyShips
                                then if myProduction > enemyProduction
                                     then (6, enemyPlanets) -- I have more ships now, and producing more: allow 0 fleets
                                     else (4, notMyPlanets) -- I have more ships now, but producing less: allow 2 fleets
                                else if myProduction > enemyProduction
                                     then (6, notMyPlanets) -- Fewer ships, but producing more: allow 0 fleets
                                     else (4, notMyPlanets) -- Fewer ships, and producing less: allow 4 fleets
                              
    source = maximumBy (comparing score) myPlanets
    sources = filter (\planet -> ships planet > 5) myPlanets
    target = minimumBy (comparing score) candidates
    
    --src = head myPlanets
    src = source
    sc2_candidates' = sortBy rank $ map (\dst -> (src,dst,score2 src dst)) candidates
   
    cumulativeShips = tail $ scanl (+) 0 $ map (\(src,dst,_) -> 1 + (ships dst)) sc2_candidates'
    
    sc2_candidates = takeWhile (\(c,_) -> c < (ships src) - 5) $ zip cumulativeShips sc2_candidates'
    
    --debugStr = show(target)
    debugStr = ("#" ++ show(sc2_candidates))
                          
    rank (_,_,a) (_,_,b) = compare b a -- descending order of score
  in 
     if 
       -- trace(debugStr) True || -- Must comment out this line when playing the TCP server
       null myPlanets || null notMyPlanets || null sc2_candidates || (not . null . drop maxFleetsM1 $ myFleets)
       then []
       else 
         --[newFleet source target (div (ships source) 2)]
         map (\(_,(src,dst,_)) -> newFleet src dst ((ships dst) + 1)) sc2_candidates

-- ---------------------------------------------------------------------

score :: Planet -> Double
score p = fromIntegral (ships p)/(1 + fromIntegral (production p))

-- ---------------------------------------------------------------------

score2 :: Planet -> Planet -> Double
score2 src dst = 
  let
    dist = fromIntegral (distance src dst)
    pSuccess = if (ships src > ships dst) then (1.0) else ( (fromIntegral (ships src - 5)) / (fromIntegral (ships dst)))
  in
   --pSuccess * fromIntegral (score dst) * (1.5^(-dist))
   (pSuccess / (score dst)) / (1.5^(dist))
  
-- ---------------------------------------------------------------------

main = playAs fAzBot

-- EOF

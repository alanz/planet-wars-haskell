{-# OPTIONS_GHC -O2 -fexcess-precision -fvia-C -optc-O3 #-}

-- Based on MyBot from Olafe, as a skeleton

import PlanetWars
import Data.Ord       (comparing)
import Data.List      (partition, minimumBy, maximumBy)
import Control.Monad  (forever, when, unless)

-- ---------------------------------------------------------------------
{-
Strategy

1. Choose an appropriate target
   a. No change for now.

2. Fire fleets from all available planets


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
                                     then (0, enemyPlanets) -- I have more ships now, and producing more
                                     else (2, notMyPlanets) -- I have more ships now, but producing less
                                else if myProduction > enemyProduction
                                     then (0, notMyPlanets) -- Fewer ships, but producing more
                                     else (4, notMyPlanets) -- Fewer ships, and producing less
                              
    source = maximumBy (comparing score) myPlanets
    sources = myPlanets
    target = minimumBy (comparing score) candidates
                          
  in 
    if null myPlanets || null notMyPlanets || null candidates -- || (not . null . drop maxFleetsM1 $ myFleets)
       then []
       else 
         --[newFleet source target (div (ships source) 2)]
         map (\src -> newFleet src target (div (ships src) 3)) sources

score :: Planet -> Double
score p = fromIntegral (ships p)/(1 + fromIntegral (production p))

main = playAs fAzBot

-- EOF

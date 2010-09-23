{-# OPTIONS_GHC -O2 -fexcess-precision -fvia-C -optc-O3 #-}

import PlanetWars
import Data.Ord       (comparing)
import Data.List      (partition, minimumBy, maximumBy)
import Control.Monad  (forever, when, unless)

dualBot :: BotMonad ()
dualBot = forever $ do 
    myPlanets    <- getMyPlanets
    unless (null myPlanets) $ do
        notMyPlanets <- getNotMyPlanets
        unless (null notMyPlanets) $ do
            enemyPlanets <- getEnemyPlanets
            myProduction <- getMyProduction; enemyProduction <- getEnemyProduction
            myShips      <- getMyShips     ; enemyShips      <- getEnemyShips
            
            let (numFleets, candidates) = if myShips > enemyShips
                                             then if myProduction > enemyProduction
                                                     then (1, enemyPlanets)
                                                     else (3, notMyPlanets)
                                             else if myProduction > enemyProduction
                                                     then (1, notMyPlanets)
                                                     else (5, notMyPlanets)
            
            unless (null candidates) $ do
                myFleets <- getMyFleets
                when (length myFleets < numFleets) $ do
                    let source = maximumBy (comparing score) myPlanets
                        target = minimumBy (comparing score) candidates
                    putFleet source target (div (ships source) 2)
    endTurn

fDualBot :: BotFunction  -- [Planet] -> [Fleet] -> [Fleet]
fDualBot planets fleets = let (myPlanets, notMyPlanets) = partition isMine planets
                              enemyPlanets    = enemy notMyPlanets
                              myProduction    = sum (map production myPlanets)
                              enemyProduction = sum (map production enemyPlanets)
                              
                              (myFleets,  enemyFleets)  = partition isMine fleets
                              myShips         = sum (map ships myPlanets)    + sum (map ships myFleets)
                              enemyShips      = sum (map ships enemyPlanets) + sum (map ships enemyFleets)
                              
                              (maxFleetsM1, candidates) = if myShips > enemyShips
                                                          then if myProduction > enemyProduction
                                                                  then (0, enemyPlanets)
                                                                  else (2, notMyPlanets)
                                                          else if myProduction > enemyProduction
                                                                  then (0, notMyPlanets)
                                                                  else (4, notMyPlanets)
                              
                              source = maximumBy (comparing score) myPlanets
                              target = minimumBy (comparing score) candidates
                          
                          in if null myPlanets || null notMyPlanets || null candidates || (not . null . drop maxFleetsM1 $ myFleets)
                                then []
                                else [newFleet source target (div (ships source) 2)]

score :: Planet -> Double
score p = fromIntegral (ships p)/(1 + fromIntegral (production p))

main = playAs fDualBot

{-# OPTIONS_GHC -O2 -fexcess-precision -fvia-C -optc-O3 #-}

import PlanetWars
import Data.Ord       (comparing)
import Data.List      (minimumBy)
import Control.Monad  (forever, forM_, unless)

rageBot :: BotMonad ()
rageBot = forever $ do
    enemyPlanets <- getEnemyPlanets
    unless (null enemyPlanets) $ do
        myPlanets <- getMyPlanets
        forM_ myPlanets $ \source -> do
            let sourceShips = ships source
            unless (sourceShips < 10*production source) $ do
                let target = minimumBy (comparing $ distance source) enemyPlanets
                putFleet source target sourceShips
    endTurn

fRageBot :: BotFunction
fRageBot planets fleets = let enemyPlanets = enemy planets
                          in if null enemyPlanets
                                then []
                                else let myPlanets = my planets
                                     in map (\(sourceShips, source) -> newFleet source (minimumBy (comparing $ distance source) enemyPlanets) sourceShips) .
                                        filter (\(sourceShips, source) -> sourceShips >= 10*production source) .
                                        zip (map ships myPlanets) $
                                            myPlanets

main = playAs fRageBot
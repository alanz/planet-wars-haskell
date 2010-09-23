{-# OPTIONS_GHC -O2 -fexcess-precision -fvia-C -optc-O3 #-}

import PlanetWars
import Control.Monad  (forever, when, unless)
import System.Random  (randomRIO)
import System.IO.Unsafe  (unsafePerformIO)

randomBot :: BotMonad ()
randomBot = forever $ do
    myPlanets <- getMyPlanets
    unless (null myPlanets) $ do
        myFleets <- getMyFleets
        when (null myFleets) $ do
            planets <- getPlanets
            let source = myPlanets !! randomBelow (length myPlanets)
                target = planets   !! randomBelow (length planets)
            putFleet source target (div (ships source) 2)
    endTurn

fRandomBot :: BotFunction
fRandomBot planets fleets = let myPlanets = my planets
                            in if null myPlanets || (not . null $ my fleets)
                                  then []
                                  else let source = myPlanets !! randomBelow (length myPlanets)
                                           target = planets   !! randomBelow (length planets)
                                       in [newFleet source target (div (ships source) 2)]

randomBelow :: Int -> Int
randomBelow n = unsafePerformIO $ randomRIO (0, n - 1)  -- SHH!  SEKRIT!

main = playAs fRandomBot

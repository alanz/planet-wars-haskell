{-# OPTIONS_GHC -O2 -fexcess-precision -fvia-C -optc-O3 #-} 

import PlanetWars
import Data.Ord       (comparing)
import Data.List      (partition, minimumBy, maximumBy)
import Control.Monad  (forever, when, unless)

prospectorBot :: BotMonad ()
prospectorBot = forever $ do
    myPlanets <- getMyPlanets
    unless (null myPlanets) $ do
        notMyPlanets <- getNotMyPlanets
        unless (null notMyPlanets) $ do
            myFleets <- getMyFleets
            when (null myFleets) $ do
                let source = maximumBy (comparing score) myPlanets
                    target = minimumBy (comparing score) notMyPlanets
                putFleet source target (div (ships source) 2)
    endTurn

fProspectorBot :: BotFunction
fProspectorBot planets fleets = let (myPlanets, notMyPlanets) = partition isMine planets
                                in if null myPlanets || null notMyPlanets || (not . null $ my fleets)
                                      then []
                                      else let source = maximumBy (comparing score) myPlanets
                                               target = minimumBy (comparing score) notMyPlanets
                                           in [newFleet source target (div (ships source) 2)]

score :: Planet -> Double
score p = fromIntegral (ships p)/(1 + fromIntegral (production p))

main = playAs fProspectorBot
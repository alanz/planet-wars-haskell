-- | Example bot using the PlanetWars module
--
module Main where

import Data.List (maximumBy, minimumBy, partition)
import qualified Data.IntMap as IM
import Data.Ord (comparing)

import PlanetWars

doTurn :: GameState  -- ^ Game state
       -> [Order]    -- ^ Orders
doTurn state = if null myFleets
                  && (not . null) myPlanets
                  && (not . null) notMyPlanets
    -- Simple ai
    then [Order (planetId strongest) (planetId weakest) ships]
    -- If we have a fleet in flight, just do nothing
    else []
  where
    myFleets = filter isAllied $ gameStateFleets state

    -- Partition all planets
    (myPlanets, notMyPlanets) = partition isAllied $
        map snd $ IM.toList $ gameStatePlanets state

    -- Find our strongest planet and the weakest neutral/hostile planet
    strongest = maximumBy (comparing planetShips) myPlanets
    weakest = minimumBy (comparing planetShips) notMyPlanets

    -- Select 2/3 of the ships
    ships = (2 * (planetShips strongest)) `div` 3

main :: IO ()
main = bot doTurn

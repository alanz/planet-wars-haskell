{-# OPTIONS_GHC -O2 -fexcess-precision -fvia-C -optc-O3 #-}

-- Based on MyBot from Olafe, as a skeleton

import PlanetWars
import Data.Ord       (comparing)
import Data.List      (partition, minimumBy, maximumBy,sortBy,foldl',sort)
import Control.Monad  (forever, when, unless)
import Debug.Trace
import qualified Data.Map as Map

-- ---------------------------------------------------------------------
{-
Strategy

1. Choose an appropriate target

1.a make some sort of impact measure, of potential gain, force needed, and time to achieve.

*2. Fire fleets from all available planets

*3. Use only the requisite force: send just enough to take the planet,
   bearing in mind the amount sent from the baddies,
   and own fleets in flight
   and growth rate x time of arrival

*-  Do not target a planet if it is already being targeted with sufficient force

-. Current targeting sometimes chooses a planet already in my hands?

-. Look at defending my own planets that are under attack
   - Perhaps include planets that will change hands in future, based on en-route fleets

-. Currently, does not attack a target if there is no chance of victory in one salvo.
   Need to look at ganging up the attcks. http://72.44.46.68/canvas?game_id=141421

- Sometimes get stuck in no-hope strategy, need to bring an element of
  randomness into the play. http://72.44.46.68/canvas?game_id=154010

- For an enemy planet, make allowance for production according to time of flight

- Do not launch from a planet that has imminent inbound enemy stuff.

- When choosing targets, rather toss the ones that are too big in
  terms of firepower, and carry on with the list

- Phase approach: Make no move in first turn. Then hit the other
  player if they have left home base undefended

- Modify the strategy when production is lower than enemy prod to make more production

- Seem to have an off by one error in attacking enemy planets, due to production
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
                              
    fp = futurePlanets planets fleets                 
                 
    sc2_candidates = concatMap (\src -> targetsForSource src fp) myPlanets
    
    --debugStr = show(target)
    --debugStr = ("#" ++ show(sc2_candidates) ++ "\n" ++ show(fp))
    debugStr = ("#" ++ show(sc2_candidates))
    --debugStr = ("#" ++ show(fp))
                          
  in 
     if 
       --trace(debugStr) False || -- Must comment out this line when playing the TCP server
       null myPlanets || null notMyPlanets || null sc2_candidates -- || (not . null . drop maxFleetsM1 $ myFleets)
       then []
       else 
         --[newFleet source target (div (ships source) 2)]
         map (\(_,(src,dst,(_,shipsDst,_))) -> newFleet src dst (shipsDst + 1)) sc2_candidates

-- ---------------------------------------------------------------------

targetsForSource
  :: Planet
     -> [(Planet, (Player, ShipCount))]
     -> [(ShipCount, (Planet, Planet, (Double, ShipCount,Int)))]
targetsForSource src fp = 
  let
    fp_candidates = map (\(dst,(owner,cnt)) -> (src,dst,score2 src dst owner cnt)) fp
    
    sc2_candidates' = sortBy rank 
                      $ filter (\(_,_,(_,shipsDst,_)) -> shipsDst < srcAvail)  
                      $ fp_candidates
   
    cumulativeShips = tail $ scanl (+) 0 $ map (\(_,_,(_,shipsDst,_)) -> 1 + shipsDst) sc2_candidates'
    
    -- TODO: Must be a more effective way
    (_,_,(_,srcShips,_)) = head $ filter (\(_,d,_) -> src == d) fp_candidates
    
    srcShips' = min srcShips (ships src)
    
    srcReserve = max 5 ((production src) * 2)
    srcAvail = srcShips' - srcReserve
    
    --debugStr = "srcShips="++show(srcShips)
    --debugStr = "targetsForSource:fp=" ++ (show_fp fp)
    debugStr = "fp_candidates="++(show_fp_candidates fp_candidates)
    --debugStr = "sc2_candidates'="++show(sc2_candidates')++"srcAvail=" ++ show(srcAvail)
    sc2_candidates = takeWhile (\(c,_) -> c < srcAvail) 
                     $ zip cumulativeShips sc2_candidates'
  
    
    rank (_,_,(a,_,_)) (_,_,(b,_,_)) = compare b a -- descending order of score

  in
   trace(debugStr)
   sc2_candidates

show_fp :: [(Planet, (Player, ShipCount))] -> [Char]
show_fp fp = concatMap (\(src,(owner,cnt)) -> "(" ++ show (planetID src) ++ "," ++ show((owner,cnt)) ++ ")") fp

show_fp_candidates :: [(Planet,Planet,(Double,ShipCount,Int))] -> [Char]
show_fp_candidates xs = concatMap (\(src,dst,(score,cnt,dist)) -> "(" ++ show(planetID src) ++ "," ++ show(planetID dst) ++ "," ++ show((truncate(score*1000000),cnt,dist)) ++ ")") xs

-- ---------------------------------------------------------------------

-- Higher score means a better/more pressing target
score2 :: Planet -> Planet -> Player -> ShipCount -> (Double,ShipCount,Int)
score2 src dst owner cnt
  | owner == Me    = (scoreMine,    cntFuture+1,dist')
  | owner == Enemy = (scoreEnemy,   cntFuture+1,dist')
  | otherwise      = (scoreNeutral, cnt+1,      dist')
  where
    
    dist = fromIntegral (distance src dst)
    dist' :: Int
    dist' = fromIntegral dist
    
    --cntFuture = cnt + (dist * (production dst))
    --cntFuture = cnt -- Can launch more fleets next time (what about production mismatch?)
    cntFuture = cnt + ((3*(dist * (production dst))) `div` 3)
    
    scoreMine    = if (cnt > 5) then (0.0) else (scoreVal 1.0 (score cntFuture dst))
    scoreEnemy   = scoreVal (pSuccess cntFuture) (score cntFuture dst)
    scoreNeutral = scoreVal (pSuccess cnt)       (score cnt dst)

    -- Utility stuff
    pSuccess shipsDst = 
      if (ships src > shipsDst) 
        then (1.0) 
        else (1.0 * ( (fromIntegral (ships src - 5)) / (fromIntegral (shipsDst))))

    --scoreVal ps s = (ps / s) / (1.5^(dist))
    scoreVal ps s = (ps / s) / (1.1^(dist))

    -- Lower score is better. Need to flip that some time
    score :: ShipCount -> Planet -> Double
    score sc p = fromIntegral (sc) / (1 + (fromIntegral (production p)**1.5))

  
-- ---------------------------------------------------------------------

futurePlanets :: [Planet] -> [Fleet] -> {-Map.Map Planet-} [(Planet, (Player, ShipCount))]
futurePlanets planets fleets =
  let
    start = foldl' (\m p -> Map.insert (p) (p,0,0) m) Map.empty planets
    
    res :: Map.Map Planet (Planet, ShipCount, ShipCount)
    res = foldl' (\m f -> updateMap m f) start fleets
    
  in
    --trace("futurePlanets:res=" ++ show(Map.elems res))
    map (\(p,m,e) -> (p,calcFuturePlanet p m e)) $ Map.elems res

calcFuturePlanet dst cntMine cntEnemy
 | isMine dst  = resMine
 | isEnemy dst = resEnemy
 | otherwise   = resNeutral
 where               
    shipsDstMine    = cntMine - cntEnemy + (ships dst)
    resMine = if (shipsDstMine < 0) then (Enemy, - shipsDstMine) else (Me, shipsDstMine)
    
    shipsDstEnemy   = cntMine - cntEnemy - (ships dst)
    resEnemy = if (shipsDstEnemy < 0) then (Enemy, - shipsDstEnemy) else (Me, shipsDstEnemy)
    
    resNeutral = calcNeutral (ships dst) cntMine cntEnemy
    
calcNeutral :: ShipCount -> ShipCount -> ShipCount -> (Player,ShipCount)    
calcNeutral neutral mine enemy = 
  let
    vals = [neutral,mine,enemy]
    minVal = minimum vals
    resVals = map (\x -> x - minVal) vals
    lastTwo = tail $ sort $ zip resVals [Neutral,Me,Enemy]
    (weakCnt,_) = head lastTwo
    (strongCnt,winner) = last lastTwo 
  in  
   (winner, strongCnt - weakCnt)
   
-- ---------------------------------------------------------------------    
    
swap (x, y) = (y, x)    

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
-- ---------------------------------------------------------------------


test =
  let
    (planets,fleets) = head $ parseGameState $ concat mapWhiteside
  in
    serializeGameTurn $ fAzBot planets fleets
   
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
  
map449 =
  [
  "P 11.6977322659 10.2756927937 0 98 4\n",
  "P 11.8819078411 19.3820370936 1 100 5\n",
  "P 11.5135566907 1.1693484938 2 100 5\n",
  "P 6.8415025020 10.1974977004 0 9 4\n",
  "P 16.5539620298 10.3538878869 0 9 4\n",
  "P 0.0000000000 4.3576188369 0 59 4\n",
  "P 23.3954645318 16.1937667505 0 59 4\n",
  "P 0.0935212635 17.8929435806 0 2 3\n",
  "P 23.3019432683 2.6584420068 0 2 3\n",
  "P 17.5328380324 0.0670805611 0 89 4\n",
  "P 5.8626264994 20.4843050262 0 89 4\n",
  "P 1.0449100440 2.3409295466 0 33 1\n",
  "P 22.3505544878 18.2104560408 0 33 1\n",
  "P 15.8613179512 16.7772678550 0 65 3\n",
  "P 7.5341465806 3.7741177323 0 65 3\n",
  "P 4.9429417536 0.0000000000 0 2 3\n",
  "P 18.4525227782 20.5513855873 0 2 3\n",
  "P 4.6934835299 5.3627481560 0 19 2\n",
  "P 18.7019810019 15.1886374313 0 19 2\n",
  "P 18.7065925350 11.7958832848 0 7 4\n",
  "P 4.6888719968 8.7555023026 0 7 4\n",
  "P 19.2998488114 2.1315039965 0 42 3\n",
  "P 4.0956157204 18.4198815908 0 42 3\n"
  ]
    
mapWhiteside = [
   "P 11.579376 11.554042 0 150 0\n",
   "P 11.667471 12.485644 2 11 5\n",
   "P 11.491280 10.622441 1 8 5\n",
   "P 6.467471 8.738467 0 56 4\n",
   "P 16.691280 14.369617 0 56 4\n",
   "P 2.343490 10.691485 1 1 3\n",
   "P 20.815261 12.416600 1 1 3\n",
   "P 17.773524 18.239086 0 15 2\n",
   "P 5.385228 4.868999 0 15 2\n",
   "P 2.868881 2.143556 0 90 2\n",
   "P 20.289870 20.964529 0 90 2\n",
   "P 18.674578 4.825621 0 60 2\n",
   "P 4.484174 18.282463 0 60 2\n",
   "P 8.853196 10.925516 1 31 5\n",
   "P 14.305555 12.182568 2 10 5\n",
   "P 16.899850 23.108085 0 76 5\n",
   "P 6.258901 0.000000 0 76 5\n",
   "P 7.054547 3.960942 0 67 3\n",
   "P 16.104204 19.147143 0 67 3\n",
   "P 0.000000 19.094381 0 57 4\n",
   "P 23.158751 4.013704 0 57 4\n",
   "P 13.804757 17.287464 0 82 5\n",
   "P 9.353994 5.820621 0 82 5\n",
   "F 2 5 14 2 4 1\n",
   "F 2 5 14 2 4 2\n",
   "F 2 5 1 2 2 1\n",
   "F 2 5 14 2 4 3\n"
   ]
  
    
-- EOF

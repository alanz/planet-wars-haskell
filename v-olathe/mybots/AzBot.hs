{-# OPTIONS_GHC -O2 -fexcess-precision -fvia-C -optc-O3 #-}

-- Based on MyBot from Olathe, as a skeleton

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

- Do not send to own planet unless it is actually in danger.

- If a planet has inbound fleets, do not launch from it if you will then lose the planet.

- Look at timing of enemy arrivals at a neutral planet, else my fleets
  knock out neutrals instead of securing planet

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
                 
    (fp',sc2_candidates') = foldl' (\(fpacc,tgtsacc) src -> accTargetsForSource src fpacc tgtsacc) (fp,[]) myPlanets
    
    sc2_candidates = sc2_candidates'
    --sc2_candidates = concatMap (\src -> targetsForSource src fp) myPlanets
    

    -- -------------------------------
    
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
         map (\(_,(src,dst,_,(_,shipsDst,_))) -> newFleet src dst shipsDst) sc2_candidates

-- ---------------------------------------------------------------------

accTargetsForSource 
  :: Planet
     -> [(Planet, (Player, ShipCount))]
     -> [(ShipCount, (Planet, Planet, Player, (Double,ShipCount,Int)))]
     -> ([(Planet, (Player, ShipCount))],[(ShipCount, (Planet, Planet, Player, (Double,ShipCount,Int)))])
accTargetsForSource src fp targets = 
  let
    targets' = targetsForSource src fp
    fp' = updateFuturePlanets fp targets'
  in
    (fp', targets' ++ targets)

-- ---------------------------------------------------------------------

targetsForSource
  :: Planet
     -> [(Planet, (Player, ShipCount))]
     -> [(ShipCount, (Planet, Planet, Player, (Double,ShipCount,Int)))]
targetsForSource src fp = 
  let
    fp_candidates = map (\(dst,(owner,cnt)) -> (src,dst,owner,score2 src dst owner cnt)) fp
    
    sc2_candidates' = sortBy rank 
                      $ filter (\(_,_,_,(score,shipsDst,_)) -> score > 0.0 && shipsDst < srcAvail)  
                      $ fp_candidates
   
    cumulativeShips = tail $ scanl (+) 0 $ map (\(_,_,_,(_,shipsDst,_)) -> 1 + shipsDst) sc2_candidates'
    
    -- TODO: Must be a more effective way
    (_,_,futureOwner,(_,srcShips,_)) = head $ filter (\(_,d,_,_) -> src == d) fp_candidates
    
    srcShips' = min srcShips (ships src)
    
    --srcReserve = max 5 ((production src) * 2)
    srcReserve = 5
    srcAvail = if (futureOwner == Me) then (srcShips' - srcReserve) else (0) -- TODO: possibly flee with all available?

    
    --debugStr = "srcShips="++show(srcShips)
    debugStr = "targetsForSource:fp=" ++ (show_fp fp)
    --debugStr = "fp_candidates="++(show_fp_candidates fp_candidates) ++ "srcShips=" ++ show(srcShips)
    --debugStr = "sc2_candidates'="++(show_fp_candidates sc2_candidates')

    sc2_candidates = takeWhile (\(c,_) -> c < srcAvail) 
                     $ zip cumulativeShips sc2_candidates'
  
    
    rank (_,_,_,(a,_,_)) (_,_,_,(b,_,_)) = compare b a -- descending order of score

  in
   --trace(debugStr)
   sc2_candidates

show_fp :: [(Planet, (Player, ShipCount))] -> [Char]
show_fp fp = concatMap (\(src,(owner,cnt)) -> "(" ++ show (planetID src) ++ "," ++ show((owner,cnt)) ++ ")") fp

show_fp_candidates :: [(Planet,Planet,Player,(Double,ShipCount,Int))] -> [Char]
show_fp_candidates xs = concatMap (\(src,dst,fo,(score,cnt,dist)) -> "(" ++ show(planetID src) ++ "," ++ show(planetID dst) ++ "," ++ "," ++ show(fo) ++ "," ++ show((truncate(score*1000000),cnt,dist)) ++ ")") xs

-- ---------------------------------------------------------------------

-- Higher score means a better/more pressing target
score2 :: Planet -> Planet -> Player -> ShipCount -> (Double,ShipCount,Int)
score2 src dst owner cnt
  | owner == Me    = (scoreMine*   1.0, cnt,        dist')
  | owner == Enemy = (scoreEnemy*  1.2, cntFuture+1,dist')
  | otherwise      = (scoreNeutral*1.2, cnt+1,      dist')
  where
    
    dist = fromIntegral (distance src dst)
    dist' :: Int
    dist' = fromIntegral dist
    
    --cntFuture = cnt + (dist * (production dst))
    --cntFuture = cnt -- Can launch more fleets next time (what about production mismatch?)
    cntFuture = cnt + ((1*(dist * (production dst))) `div` 4)
    
    -- Higher is more desirable target, runs from 0 up
    --scoreMine    = if (cnt > 5) then (0.0) else  ((scoreVal 1.0 (score cntFuture dst)))
    scoreMine    = 0.0
    scoreEnemy   = scoreVal (pSuccess cntFuture) (score cntFuture dst)
    scoreNeutral = scoreVal (pSuccess cnt)       (score cnt dst)

    -- Utility stuff
    pSuccess shipsDst = 
      if (ships src > shipsDst) 
        then (1.0) 
        else (1.0 * ( (fromIntegral (ships src - 5)) / (fromIntegral (shipsDst))))

    --scoreVal ps s = (ps / s) / (1.5^(dist))
    scoreVal ps s = (ps / s) / (1.3^(dist))
    --scoreVal ps s = (ps / s) / (1.1^(dist))

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

-- ---------------------------------------------------------------------

calcFuturePlanet
  :: (HasOwner a, HasShips a) =>
     a -> ShipCount -> ShipCount -> (Player, ShipCount)
calcFuturePlanet dst cntMine cntEnemy
 | isMine dst  = resMine
 | isEnemy dst = resEnemy
 | otherwise   = resNeutral
 where               
    shipsDstMine = cntMine - cntEnemy + (ships dst)
    resMine = if (shipsDstMine < 0) then (Enemy, - shipsDstMine) else (Me, shipsDstMine)
    
    shipsDstEnemy   = cntMine - cntEnemy - (ships dst)
    resEnemy = if (shipsDstEnemy < 0) then (Enemy, - shipsDstEnemy) else (Me, shipsDstEnemy)
    
    resNeutral = calcNeutral (ships dst) cntMine cntEnemy
    
-- ---------------------------------------------------------------------    

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

updateFuturePlanets
  :: [(Planet, (Player, ShipCount))] -- fp
  -> [(ShipCount, (Planet, Planet, Player, (Double,ShipCount,Int)))] -- targets
  -> [(Planet, (Player, ShipCount))]  -- fp'
updateFuturePlanets fp targets 
  | targets == [] = fp
  | otherwise     = fp'
  where
    
    effect :: Map.Map Planet ShipCount
    effect = foldl' (\m (_,(src,dst,_,(_,cntMine,_))) -> updateEffect m src dst cntMine) Map.empty targets
    
    fpm = foldl' (\fm (p,cnt) -> updateFpm fm p cnt) (Map.fromList fp) $ Map.assocs effect
    
    updateFpm m planet cnt 
      | owner == Me    = Map.insert planet resMine    m
      | owner == Enemy = Map.insert planet resEnemy   m
      | otherwise      = Map.insert planet resNeutral m
      where               
         (owner,currentCnt) = m Map.! planet
         
         shipsDstMine = currentCnt + cnt
         resMine = if (shipsDstMine < 0) then (Enemy, - shipsDstMine) else (Me, shipsDstMine)
    
         shipsDstEnemy   = currentCnt - cnt
         resEnemy = if (shipsDstEnemy < 0) then (Enemy, - shipsDstEnemy) else (Me, shipsDstEnemy)
    
         resNeutral = calcNeutral currentCnt cnt 0


    -- res = foldl' (\m f -> updateFp m f) effect targets

    fp' = 
      --trace ("updateFuturePlanets:effect=" ++ show(effect))
      --trace ("updateFuturePlanets:fpm=" ++ show(Map.assocs fpm))
      Map.assocs fpm
    
-- ---------------------------------------------------------------------    

updateEffect m src dst cntMine =
  let
    m' = updateOne m src (-cntMine)
    m'' = updateOne m' dst cntMine
    
    updateOne m p cnt
      | Map.member p m = Map.insert p (cnt + (m Map.! p)) m
      | otherwise      = Map.insert p (cnt              ) m
  in 
   m''
   
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
    (planets,fleets) = head $ parseGameState $ concat mapStr
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
  
map273440 =
  [
  "P 11.015864 11.795797 0 140 1\n",
  "P 3.478234 22.162737 1 42 5\n",
  "P 18.553495 1.428856 2 15 5\n",
  "P 14.408651 0.000000 0 26 3\n",
  "P 7.623077 23.591593 0 26 3\n",
  "P 13.165319 6.853138 0 39 2\n",
  "P 8.866410 16.738455 0 39 2\n",
  "P 12.019278 5.208630 0 17 4\n",
  "P 10.012450 18.382963 0 17 4\n",
  "P 13.449553 16.621267 0 25 5\n",
  "P 8.582176 6.970326 0 25 5\n",
  "P 2.377923 15.590557 1 1 5\n",
  "P 19.653806 8.001036 2 1 5\n",
  "P 16.740968 4.853610 2 16 5\n",
  "P 5.290761 18.737983 1 16 5\n",
  "P 0.000000 17.234418 0 19 1\n",
  "P 22.031729 6.357175 0 19 1\n",
  "P 16.658689 19.463701 0 28 5\n",
  "P 5.373040 4.127892 0 28 5\n",
  "P 19.520596 21.548997 0 83 2\n",
  "P 2.511132 2.042596 0 83 2\n",
  "P 3.826406 8.028899 0 60 5\n",
  "P 18.205323 15.562695 0 60 5\n",
  "F 2 18 2 7 8 1\n",
  "F 1 18 1 8 8 2\n",
  "F 2 27 2 3 5 2\n"
  ]

map275219_30 =
  [
  "P 10.963093 10.255790 1 24 5\n",
  "P 18.766531 18.357256 2 11 5\n",
  "P 3.159656 2.154324 1 25 5\n",
  "P 21.241354 6.768781 0 4 2\n",
  "P 0.684833 13.742798 1 35 2\n",
  "P 6.455141 15.918761 1 10 1\n",
  "P 15.471046 4.592819 1 8 1\n",
  "P 14.149567 20.511580 2 51 5\n",
  "P 7.776620 0.000000 1 41 5\n",
  "P 16.572712 14.102863 2 7 2\n",
  "P 5.353475 6.408717 0 74 2\n",
  "P 0.000000 5.921174 0 55 4\n",
  "P 21.926187 14.590406 2 30 4\n",
  "P 13.399414 11.523454 0 43 3\n",
  "P 8.526772 8.988126 0 67 3\n",
  "P 1.571664 3.698954 1 63 4\n",
  "P 20.354523 16.812626 2 11 4\n",
  "P 0.370913 15.413870 0 88 1\n",
  "P 21.555274 5.097710 0 88 1\n",
  "P 6.474049 4.243127 1 27 3\n",
  "P 15.452137 16.268453 2 9 3\n",
  "P 11.278773 3.383705 0 49 3\n",
  "P 10.647414 17.127874 0 49 3\n",
  "F 1 31 8 3 16 2\n",
  "F 1 30 15 7 21 8\n",
  "F 2 7 20 3 12 2\n",
  "F 2 9 16 3 11 1\n",
  "F 2 11 1 3 12 2\n",
  "F 1 24 0 3 11 1\n",
  "F 2 5 20 3 12 3\n",
  "F 2 8 1 3 12 3\n",
  "F 2 6 16 3 11 2\n",
  "F 2 7 12 13 10 3\n",
  "F 2 7 16 13 9 2\n",
  "F 2 9 1 13 9 2\n",
  "F 2 9 1 13 9 4\n",
  "F 2 8 16 13 9 4\n",
  "F 2 6 20 13 6 1\n",
  "F 2 7 1 13 9 5\n",
  "F 2 6 16 13 9 5\n",
  "F 2 5 16 22 10 7\n",
  "F 2 6 1 22 9 6\n",
  "F 2 6 20 22 5 2\n",
  "F 2 5 9 22 7 4\n",
  "F 2 6 1 22 9 7\n",
  "F 1 28 8 22 18 16\n",
  "F 1 14 0 13 3 1\n",
  "F 2 5 1 22 9 8\n",
  "F 2 6 20 22 5 4\n",
  "F 2 6 16 22 10 9\n",
  "F 2 5 9 22 7 6\n",
  "F 1 19 6 3 7 6\n",
  "F 1 17 5 22 5 4\n",
  "F 1 22 2 22 17 16\n"
  ]
    
-- EOF

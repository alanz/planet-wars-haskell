{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, Rank2Types, TypeSynonymInstances #-}
{-# OPTIONS_GHC -O2 -fexcess-precision -fvia-C -optc-O3 #-}

-- PlanetWars library for Google AI Challenge of late 2010.
-- Optimized for speed so that your bot code has that much more time to do fun things to win the contest.

module PlanetWars ( Player(Neutral, Me, Enemy), isMine, isNotMine, isNeutral, isEnemy, my, notMy, neutral, enemy
                  , ShipCount
                  , TurnCount
                  , HasOwner, owner
                  , HasShips, ships
                  , Planet, production, distance, planetID
                  , Fleet, newFleet, source, target, turnsLeft
                  , Bot
                  , BotFunction  -- [Planet] -> [Fleet] -> [Fleet]
                  , BotMonad
  {- [Planet]  -} , getPlanets, getMyPlanets, getNotMyPlanets, getNeutralPlanets, getEnemyPlanets
  {- [Fleet]   -} , getFleets, getMyFleets, getEnemyFleets
  {- ShipCount -} , getShips, getMyShips, getNotMyShips, getNeutralShips, getEnemyShips
  {- ShipCount -} , getProduction, getMyProduction, getNotMyProduction, getNeutralProduction, getEnemyProduction
  {- ()        -} , putFleet, endTurn
                  , playAs, playAs'
                  -- For other purposes                            
                  , parseGameState                            
                  , Position(..), position
                  , PlanetID(..)                
                    
                  ) where

import Data.Int    (Int16, Int32)
import Data.Word   (Word8, Word16)

import Data.List   (isPrefixOf)
import Data.Maybe  (fromMaybe, fromJust)
import Data.Char   (ord)
import GHC.Show    (showSpace)

import Data.Ix            (Ix)
import Data.Array.MArray  (newListArray, readArray, writeArray, getBounds, getElems)
import Data.Array.ST      (STArray)

import Control.Monad          (forM, mapM_)
import Control.Monad.ST.Lazy  (ST, runST)

import System.IO    (stdin, stdout, getContents, hFlush, hSetBuffering, BufferMode(NoBuffering, BlockBuffering))
import System.Info  (os)

import qualified StringLike                 as S  (StringLike, null, uncons, tail, dropWhile, readIntegral, readFractional)
import qualified Data.ByteString.Lazy.Char8 as L  (ByteString, getContents)


-- ===================== --
-- == Game data types == --
-- ===================== --

-- Use IntNN instead of WordNN for exported types to ensure correct results
--   if people make complex AI-related formulas involving subtraction.
-- Use WordNN instead of IntNN for nonexported types to not waste all those
--   extra positive values, since we'll ensure subtraction is done right in
--   this library.

-----------------
-- Player type --
-----------------
data Player = Neutral | Me | Enemy
  deriving (Show, Ix, Eq, Ord)

{-# SPECIALIZE isNeutral :: Planet -> Bool #-}
{-# SPECIALIZE isNeutral :: Fleet  -> Bool #-}
isNeutral :: HasOwner a => a -> Bool; isNeutral = (== Neutral) . owner
{-# SPECIALIZE isMine    :: Planet -> Bool #-}
{-# SPECIALIZE isMine    :: Fleet  -> Bool #-}
isMine    :: HasOwner a => a -> Bool; isMine    = (== Me)      . owner
{-# SPECIALIZE isNotMine :: Planet -> Bool #-}
{-# SPECIALIZE isNotMine :: Fleet  -> Bool #-}
isNotMine :: HasOwner a => a -> Bool; isNotMine = (/= Me)      . owner
{-# SPECIALIZE isEnemy   :: Planet -> Bool #-}
{-# SPECIALIZE isEnemy   :: Fleet  -> Bool #-}
isEnemy   :: HasOwner a => a -> Bool; isEnemy   = (== Enemy)      . owner

{-# SPECIALIZE neutral :: [Planet] -> [Planet] #-}
{-# SPECIALIZE neutral :: [Fleet]  -> [Fleet]  #-}
neutral :: HasOwner a => [a] -> [a]; neutral = filter isNeutral
{-# SPECIALIZE my      :: [Planet] -> [Planet] #-}
{-# SPECIALIZE my      :: [Fleet]  -> [Fleet]  #-}
my      :: HasOwner a => [a] -> [a]; my      = filter isMine
{-# SPECIALIZE notMy   :: [Planet] -> [Planet] #-}
{-# SPECIALIZE notMy   :: [Fleet]  -> [Fleet]  #-}
notMy   :: HasOwner a => [a] -> [a]; notMy   = filter isNotMine
{-# SPECIALIZE enemy   :: [Planet] -> [Planet] #-}
{-# SPECIALIZE enemy   :: [Fleet]  -> [Fleet]  #-}
enemy   :: HasOwner a => [a] -> [a]; enemy   = filter isEnemy

--------------------
-- ShipCount type --
--------------------
newtype ShipCount = ShipCount Int32
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

--------------------
-- TurnCount type --
--------------------
newtype TurnCount = TurnCount Int16
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

-------------------
-- RawShips type --
-------------------
class HasOwner a where
  owner :: a -> Player

class HasShips a where
  ships :: a -> ShipCount

-----------------
-- Planet type --
-----------------
data Planet = Planet {-# UNPACK #-} !Player {-# UNPACK #-} !ShipCount !RawPlanet  -- Do not UNPACK RawPlanet so that our caching of RawPlanets in the parser below saves us space and copying time.
  deriving (Show, Eq, Ord)

instance HasOwner Planet where
    {-# SPECIALIZE owner :: Planet -> Player #-}
    owner (Planet x _ _) = x
instance HasShips Planet where
    {-# SPECIALIZE ships :: Planet -> ShipCount #-}
    ships (Planet _ x _) = x

rawPlanet  :: Planet -> RawPlanet; rawPlanet  (Planet _ _ x) = x
production :: Planet -> ShipCount; production = rawProduction . rawPlanet
position   :: Planet -> Position ; position   = rawPosition   . rawPlanet
planetID   :: Planet -> PlanetID ; planetID   = rawPlanetID   . rawPlanet

distance :: Planet -> Planet -> TurnCount
distance !a !b = rawDistance (position a) (position b)

data RawPlanet = RawPlanet {-# UNPACK #-} !Position {-# UNPACK #-} !ShipCount {-# UNPACK #-} !PlanetID
  deriving (Show, Ord)
rawPosition   :: RawPlanet -> Position ; rawPosition   (RawPlanet x _ _) = x
rawProduction :: RawPlanet -> ShipCount; rawProduction (RawPlanet _ x _) = x
rawPlanetID   :: RawPlanet -> PlanetID ; rawPlanetID   (RawPlanet _ _ x) = x

instance Eq RawPlanet where
  (RawPlanet _ _ (PlanetID a)) == (RawPlanet _ _ (PlanetID b)) = a == b

newtype PlanetID = PlanetID Word8
  deriving (Show, Ix, Eq, Ord)

data Position = Position {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Show, Eq, Ord)

{-# INLINE rawDistance #-}
rawDistance :: Position -> Position -> TurnCount
rawDistance (Position x1 y1) (Position x2 y2) = let dx = x1 - x2
                                                    dy = y1 - y2
                                                in ceiling $! sqrt $! dx*dx + dy*dy

-----------------
-- Fleet type --
-----------------
data Fleet = Fleet {-# UNPACK #-} !Player {-# UNPACK #-} !ShipCount TurnCount !Planet !Planet  -- Do not make TurnCount strict, so that we can avoid calculating it when someone sends a Fleet and never uses it.
  deriving (Show, Eq, Ord)

instance HasOwner Fleet where
    {-# SPECIALIZE owner :: Fleet -> Player #-}
    owner (Fleet x _ _ _ _) = x
instance HasShips Fleet where
    {-# SPECIALIZE ships :: Fleet -> ShipCount #-}
    ships (Fleet _ x _ _ _) = x

turnsLeft :: Fleet -> TurnCount; turnsLeft (Fleet _ _ x _ _) = x
target    :: Fleet -> Planet   ; target    (Fleet _ _ _ x _) = x
source    :: Fleet -> Planet   ; source    (Fleet _ _ _ _ x) = x

newFleet :: Planet -> Planet -> ShipCount -> Fleet
newFleet source target ships = Fleet Me ships (distance source target) target source


-- =============== --
-- == Bot types == --
-- =============== --

class Bot a where
  runBot :: a -> [BotInput] -> [BotOutput]

type BotInput  = ([Planet], [Fleet])
type BotOutput = [Fleet]

--------------------------
-- Functional-style bot --
--------------------------
instance Bot BotFunction where
  runBot = map . uncurry

type BotFunction = [Planet] -> [Fleet] -> [Fleet]

--------------------------
-- Imperative-style bot --
--------------------------

instance Bot (BotMonad a) where
  runBot _ []            = []
  runBot m ((ps, fs):is) = case runBotMonad m (BMIState ps fs (Just is)) [] [] of
                               (_, _, o, os) -> o:os

-- Meld two monads: state and reverse state (http://lukepalmer.wordpress.com/2008/08/10/mindfuck-the-reverse-state-monad/)
instance Monad BotMonad where
  return x = BotMonad $ \is o os -> (x, is, o, os)
  m >>= f  = BotMonad $ \is o os -> let (a, is',  o'', os'') = runBotMonad m is o' os'
                                        (b, is'', o',  os')  = case is' of
                                            BMIState _ _ Nothing -> (undefined, is', o, os)
                                            _                    -> runBotMonad (f a) is' o os
                                    in (b, is'', o'', os'')

newtype BotMonad a = BotMonad { runBotMonad :: BMIState -> BotOutput -> [BotOutput] -> (a, BMIState, BotOutput, [BotOutput]) }
data BMIState = BMIState [Planet] [Fleet] !(Maybe [BotInput])

getPlanets        :: BotMonad [Planet]; getPlanets        = BotMonad $ \is@(BMIState ps _ _) o os -> (        ps, is, o, os)
getNeutralPlanets :: BotMonad [Planet]; getNeutralPlanets = BotMonad $ \is@(BMIState ps _ _) o os -> (neutral ps, is, o, os)
getMyPlanets      :: BotMonad [Planet]; getMyPlanets      = BotMonad $ \is@(BMIState ps _ _) o os -> (my      ps, is, o, os)
getNotMyPlanets   :: BotMonad [Planet]; getNotMyPlanets   = BotMonad $ \is@(BMIState ps _ _) o os -> (notMy   ps, is, o, os)
getEnemyPlanets   :: BotMonad [Planet]; getEnemyPlanets   = BotMonad $ \is@(BMIState ps _ _) o os -> (enemy   ps, is, o, os)

getFleets        :: BotMonad [Fleet]; getFleets        = BotMonad $ \is@(BMIState _ fs _) o os -> (        fs, is, o, os)
getMyFleets      :: BotMonad [Fleet]; getMyFleets      = BotMonad $ \is@(BMIState _ fs _) o os -> (my      fs, is, o, os)
getEnemyFleets   :: BotMonad [Fleet]; getEnemyFleets   = BotMonad $ \is@(BMIState _ fs _) o os -> (enemy   fs, is, o, os)

------------------
-- Calculations --
------------------
getShips :: BotMonad ShipCount
getShips = BotMonad $ \is@(BMIState ps fs _) o os ->
    (sum (map ships ps) + sum (map ships fs), is, o, os)

getNeutralShips :: BotMonad ShipCount
getNeutralShips = BotMonad $ \is@(BMIState ps fs _) o os ->
    (sum (map ships $ neutral ps), is, o, os)

getMyShips :: BotMonad ShipCount
getMyShips = BotMonad $ \is@(BMIState ps fs _) o os ->
 (sum (map ships $ my ps) + sum (map ships $ my fs), is, o, os)

getNotMyShips :: BotMonad ShipCount
getNotMyShips = BotMonad $ \is@(BMIState ps fs _) o os ->
 (sum (map ships $ notMy ps) + sum (map ships $ notMy fs), is, o, os)

getEnemyShips :: BotMonad ShipCount
getEnemyShips = BotMonad $ \is@(BMIState ps fs _) o os ->
 (sum (map ships $ enemy ps) + sum (map ships $ enemy fs), is, o, os)

getProduction :: BotMonad ShipCount
getProduction = BotMonad $ \is@(BMIState ps _ _) o os ->
 (sum (map production ps), is, o, os)

getNeutralProduction :: BotMonad ShipCount
getNeutralProduction = BotMonad $ \is@(BMIState ps _ _) o os ->
 (sum (map production $ neutral ps), is, o, os)

getMyProduction :: BotMonad ShipCount
getMyProduction = BotMonad $ \is@(BMIState ps _ _) o os ->
 (sum (map production $ my ps), is, o, os)

getNotMyProduction :: BotMonad ShipCount
getNotMyProduction = BotMonad $ \is@(BMIState ps _ _) o os ->
 (sum (map production $ notMy ps), is, o, os)

getEnemyProduction :: BotMonad ShipCount
getEnemyProduction = BotMonad $ \is@(BMIState ps _ _) o os ->
 (sum (map production $ enemy ps), is, o, os)

putFleet :: Planet -> Planet -> ShipCount -> BotMonad ()
putFleet source target ships = BotMonad $ \is o os -> ((), is, newFleet source target ships:o, os)

endTurn :: BotMonad ()
endTurn = BotMonad $ \(BMIState _ _ (Just iis)) o os -> ( ()
                                                        , case iis of 
                                                               []          -> BMIState undefined undefined Nothing
                                                               (ps, fs):is -> BMIState ps        fs        (Just is)
                                                        , []
                                                        , o:os)


-- ================= --
-- == IO handling == --
-- ================= --

{-# SPECIALIZE playAs :: BotFunction -> IO () #-}
{-# SPECIALIZE playAs :: BotMonad a  -> IO () #-}
playAs :: Bot a => a -> IO ()
playAs bot = do
    hSetBuffering   stdin  NoBuffering                   -- No buffering for zero lag time
    hSetBuffering   stdout (BlockBuffering (Just 4096))  -- Block buffering with hFlush for quicker output
    case os of
        ('m':'i':'n':'g':'w':_) -> do input <- getContents  -- L.getContents doesn't work on Windows (http://hackage.haskell.org/trac/ghc/ticket/806#comment:12)
                                      mapM_ (\m -> putStr m >> hFlush stdout) (playAs' bot input)  -- hFlush needed: Windows waits a long time to flush.
        _                       -> do input <- L.getContents
                                      mapM_ (\m -> putStr m >> hFlush stdout) (playAs' bot input)

{-# SPECIALIZE playAs' :: BotFunction -> L.ByteString -> [String] #-}
{-# SPECIALIZE playAs' :: BotMonad a  -> L.ByteString -> [String] #-}
playAs' :: (Bot a, S.StringLike string) => a -> string -> [String]
playAs' bot = map serializeGameTurn . runBot bot . parseGameState

serializeGameTurn :: [Fleet] -> String
serializeGameTurn fs = foldr (\x xs -> serializeGameMove x . xs) (showString "go\n") fs ""

serializeGameMove :: Fleet -> ShowS
serializeGameMove f = let PlanetID  !src = planetID . source $ f
                          PlanetID  !tgt = planetID . target $ f
                          ShipCount !cnt = ships f
                      in case src == tgt || cnt <= 0 of
                            True  -> id  -- If src == tgt, Planet Wars test server (http://www.benzedrine.cx/planetwars/) drops connection.
                            False -> shows src . showSpace . shows tgt . showSpace . shows cnt . showChar '\n'

{-# SPECIALIZE parseGameState :: L.ByteString -> [([Planet], [Fleet])] #-}
parseGameState :: S.StringLike string => string -> [([Planet], [Fleet])]
parseGameState xs | S.null xs = []
                  | otherwise = runST $ do (psA, ps, ys) <- parsePlanetsFirstTime xs
                                           (fs, zs)      <- parseFleets     psA   ys
                                           pfs           <- parseGameState' psA . parseGo $ zs
                                           return $ (ps, fs):pfs
  where
    {-# SPECIALIZE parseGameState' :: STArray s PlanetID Planet -> L.ByteString -> ST s [([Planet], [Fleet])] #-}
    parseGameState' :: S.StringLike string => STArray s PlanetID Planet -> string -> ST s [([Planet], [Fleet])]
    parseGameState' psA xs | S.null xs = return []
    parseGameState' psA xs | otherwise = do (ps, ys) <- parsePlanets    psA xs
                                            (fs, zs) <- parseFleets     psA ys
                                            pfs      <- parseGameState' psA . parseGo $ zs
                                            return $ (ps, fs):pfs
    
    {-# SPECIALIZE parsePlanetsFirstTime :: L.ByteString -> ST s (STArray s PlanetID Planet, [Planet], L.ByteString) #-}
    parsePlanetsFirstTime :: S.StringLike string => string -> ST s (STArray s PlanetID Planet, [Planet], string)
    parsePlanetsFirstTime xs = do let (ps, pid, ys) = fromJust . parseIt (PlanetID 0) $ xs
                                  psA <- newListArray (PlanetID 0, pid) ps
                                  return (psA, ps, ys)
       where
         {-# SPECIALIZE parseIt :: PlanetID -> L.ByteString -> Maybe ([Planet], PlanetID, L.ByteString) #-}
         parseIt :: S.StringLike string => PlanetID -> string -> Maybe ([Planet], PlanetID, string)
         parseIt pid@(PlanetID rpid) bs = do
             ('P'       , cs) <- S.uncons         bs
             (' '       , ds) <- S.uncons         cs
             (x         , es) <- S.readFractional ds
             (' '       , fs) <- S.uncons         es
             (y         , gs) <- S.readFractional fs
             (' '       , hs) <- S.uncons         gs
             (owner     , is) <- S.uncons         hs
             (' '       , js) <- S.uncons         is
             (ships     , ks) <- S.readIntegral   js
             (' '       , ls) <- S.uncons         ks
             (production, ms) <- S.readIntegral   ls
             ('\n'      , ns) <- S.uncons         ms
             let (ys, pid'  , os) = fromMaybe ([], pid, ns) . parseIt (PlanetID (succ rpid)) $ ns
                 owner'           =  case owner of
                                          '0' -> Neutral
                                          '1' -> Me
                                          '2' -> Enemy
             return (Planet owner' (ShipCount ships) (RawPlanet (Position x y) (ShipCount production) pid):ys, pid', os)
    
    {-# SPECIALIZE parsePlanets :: STArray s PlanetID Planet -> L.ByteString -> ST s ([Planet], L.ByteString) #-}
    parsePlanets :: S.StringLike string => STArray s PlanetID Planet -> string -> ST s ([Planet], string)
    parsePlanets psA xxs = do bounds <- getBounds psA
                              let PlanetID last = snd bounds
                                  (weakPs, xs)  = fromJust . parseIt $ xxs
                              ps <- forM (zip (map PlanetID [0..last]) weakPs) $
                                         \(pid, (owner, ships)) -> do
                                             Planet _ _ rawP <- readArray psA pid
                                             let p = Planet owner ships rawP
                                             writeArray psA pid p
                                             return p
                              return (ps, xs)
      where
        {-# SPECIALIZE parseIt :: L.ByteString -> Maybe ([(Player, ShipCount)], L.ByteString) #-}
        parseIt :: S.StringLike string => string -> Maybe ([(Player, ShipCount)], string)
        parseIt bs = do
             ('P'       , cs) <- S.uncons              bs
             (' '       , ds) <- S.uncons              cs
             let ({-x-}   es) =  S.dropWhile (/= ' ')  ds
             (' '       , fs) <- S.uncons              es
             let ({-y-}   gs) =  S.dropWhile (/= ' ')  fs
             (' '       , hs) <- S.uncons              gs
             (owner     , is) <- S.uncons              hs
             (' '       , js) <- S.uncons              is
             (ships     , ks) <- S.readIntegral        js
             (' '       , ls) <- S.uncons              ks
             let ({-prd-} ms) =  S.dropWhile (/= '\n') ls
             ('\n'      , ns) <- S.uncons              ms
             let (ys, os) = fromMaybe ([], ns) . parseIt $ ns
                 owner'   =  case owner of
                                  '0' -> Neutral
                                  '1' -> Me
                                  '2' -> Enemy
             return ((owner', ShipCount ships):ys, os)
    
    {-# SPECIALIZE parseFleets :: STArray s PlanetID Planet -> L.ByteString -> ST s ([Fleet], L.ByteString) #-}
    parseFleets :: S.StringLike string => STArray s PlanetID Planet -> string -> ST s ([Fleet], string)
    parseFleets psA xxs = do let (rawFs, xs) = fromMaybe ([], xxs) . parseIt $ xxs
                             fs <- forM rawFs $ \(owner, ships, turnsLeft, target, source) -> do
                                 target' <- readArray psA target
                                 source' <- readArray psA source
                                 return $ Fleet owner ships turnsLeft target' source'
                             return (fs, xs)
      where
        {-# SPECIALIZE parseIt :: L.ByteString -> Maybe ([(Player, ShipCount, TurnCount, PlanetID, PlanetID)], L.ByteString) #-}
        parseIt :: S.StringLike string => string -> Maybe ([(Player, ShipCount, TurnCount, PlanetID, PlanetID)], string)
        parseIt bs = do
             ('F'       , cs) <- S.uncons             bs
             (' '       , ds) <- S.uncons             cs
             (owner     , es) <- S.uncons             ds
             (' '       , fs) <- S.uncons             es
             (ships     , gs) <- S.readIntegral       fs
             (' '       , hs) <- S.uncons             gs
             (source    , is) <- S.readIntegral       hs
             (' '       , js) <- S.uncons             is
             (target    , ks) <- S.readIntegral       js
             (' '       , ls) <- S.uncons             ks
             let ({-tot-} ms) =  S.dropWhile (/= ' ') ls
             (' '       , ns) <- S.uncons             ms
             (turnsLeft , os) <- S.readIntegral       ns
             ('\n'      , ps) <- S.uncons             os
             let (ys, qs) = fromMaybe ([], ps) . parseIt $ ps
                 owner'   =  case owner of
                                  '0' -> Neutral
                                  '1' -> Me
                                  '2' -> Enemy
             return ((owner', ShipCount ships, TurnCount turnsLeft, PlanetID target, PlanetID source):ys, qs)
    
    {-# SPECIALIZE parseGo :: L.ByteString -> L.ByteString #-}
    parseGo :: S.StringLike string => string -> string
    parseGo bs = fromJust $ do
        ('g' , cs) <- S.uncons bs
        ('o' , ds) <- S.uncons cs
        ('\n', es) <- S.uncons ds
        return es
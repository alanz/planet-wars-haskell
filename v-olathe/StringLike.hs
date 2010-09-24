{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -O2 -fexcess-precision -fvia-C -optc-O3 #-}

module StringLike (StringLike, null, uncons, tail, dropWhile, readIntegral, readFractional) where

import Data.Char   (ord)
import Data.Ratio  ((%))

import           Prelude                  hiding  (null, tail, dropWhile)
import qualified Prelude                    as P  (null, tail, dropWhile)
import qualified Data.ByteString.Lazy.Char8 as L  (ByteString, null, uncons, tail, dropWhile, readInteger)

class Show a => StringLike a where
  null      :: a -> Bool
  uncons    :: a -> Maybe (Char, a)
  tail      :: a -> a
  dropWhile :: (Char -> Bool) -> a -> a
  readIntegral   :: Integral   i => a -> Maybe (i, a)
  readFractional :: Fractional f => a -> Maybe (f, a)

instance StringLike String where
  {-# SPECIALIZE null :: String -> Bool #-}
  null = P.null
  
  {-# SPECIALIZE uncons :: String -> Maybe (Char, String) #-}
  uncons (x:xs) = Just (x, xs)
  uncons _      = Nothing
  
  {-# SPECIALIZE tail :: String -> String #-}
  tail = P.tail
  
  {-# SPECIALIZE dropWhile :: (Char -> Bool) -> String -> String #-}
  dropWhile = P.dropWhile
  
  {-# INLINE readIntegral #-}
  readIntegral = readIntegral'
  
  {-# INLINE readFractional #-}
  readFractional = readFractional'

instance StringLike L.ByteString where
  {-# SPECIALIZE null :: L.ByteString -> Bool #-}
  null = L.null
  
  {-# SPECIALIZE uncons :: L.ByteString -> Maybe (Char, L.ByteString) #-}
  uncons = L.uncons
  
  {-# SPECIALIZE tail :: L.ByteString -> L.ByteString #-}
  tail = L.tail
  
  {-# SPECIALIZE dropWhile :: (Char -> Bool) -> L.ByteString -> L.ByteString #-}
  dropWhile = L.dropWhile
  
  {-# INLINE readIntegral #-}
  readIntegral = readIntegral'
  
  {-# INLINE readFractional #-}
  readFractional = readFractional'

{-# SPECIALIZE readIntegral' :: Integral i => String       -> Maybe (i, String)       #-}
{-# SPECIALIZE readIntegral' :: Integral i => L.ByteString -> Maybe (i, L.ByteString) #-}
readIntegral' :: (StringLike string, Integral i) => string -> Maybe (i, string)
readIntegral' bs = do
    (c, cs) <- uncons bs
    case c of
         '+' -> handleAfterSign (+) cs
         '-' -> handleAfterSign (-) cs
         _   -> case c >= '0' && c <= '9' of
                     True -> handleAfterFirstDigit (+) (fromIntegral (ord c - ord '0')) cs
  where
    handleAfterSign :: (StringLike string, Integral i) => (i -> i -> i) -> string -> Maybe (i, string)
    handleAfterSign op bs = do
        (c, cs) <- uncons bs
        case c >= '0' && c <= '9' of
             True -> handleAfterFirstDigit op (fromIntegral (ord c - ord '0')) cs
    
    handleAfterFirstDigit :: (StringLike string, Integral i) => (i -> i -> i) -> i -> string -> Maybe (i, string)
    handleAfterFirstDigit op res bs =
        case uncons bs of
             Nothing      -> Just (res, bs)
             Just (c, cs) -> case c >= '0' && c <= '9' of
                                  True  -> handleAfterFirstDigit op ((10*res) `op` fromIntegral (ord c - ord '0')) cs
                                  False -> Just (res, bs)

{-# SPECIALIZE readFractional' :: Fractional f => String       -> Maybe (f, String)       #-}
{-# SPECIALIZE readFractional' :: Fractional f => L.ByteString -> Maybe (f, L.ByteString) #-}
readFractional' :: (StringLike string, Fractional f) => string -> Maybe (f, string)
readFractional' bs = do
    (beforeDot  , cs) <- readIntegral bs
    (possiblyDot, ds) <- uncons       cs
    case possiblyDot of
         '.' -> do let op = case beforeDot < 0 of
                                 True  -> (-)
                                 False -> (+)
                       (mantissa, exp1, es) = handleAfterDot op beforeDot 0 ds
                   (possiblyE, fs) <- uncons es
                   case possiblyE of
                        'E' -> do (exp2, gs) <- readIntegral fs
                                  let exp = exp1 + exp2
                                  case exp > 0 of
                                       True  -> return (fromInteger  (mantissa*10^  exp ), gs)
                                       False -> return (fromRational (mantissa%10^(-exp)), gs)
                        _   -> case exp1 > 0 of
                                     True  -> return (fromInteger  (mantissa*10^  exp1 ), es)
                                     False -> return (fromRational (mantissa%10^(-exp1)), es)
         _   -> return (fromInteger beforeDot, cs)
    where
      handleAfterDot :: StringLike string => (Integer -> Integer -> Integer) -> Integer -> Int -> string -> (Integer, Int, string)
      handleAfterDot op mant exp xxs =
          case uncons xxs of
               Nothing      -> (mant, exp, xxs)
               Just (x, xs) -> case x >= '0' && x <= '9' of
                                    True  -> handleAfterDot op ((10*mant) `op` toInteger (ord x - ord '0')) (exp - 1) xs
                                    False -> (mant, exp, xxs)

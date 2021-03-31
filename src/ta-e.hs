{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O2 #-}

import Control.Applicative
import Control.Arrow (second)
import Control.Monad.ST (ST, runST)
import Control.Monad.State.Strict
import qualified Data.Bits as BT
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (isSpace)
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as M
import Data.List (foldl')
import Data.Maybe (fromJust, fromMaybe)
import Data.STRef.Strict (modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)
import Debug.Trace

solve :: Int -> U.Vector (Int, Int, Int) -> Int
solve n rs = runST $ do
  mref <- newSTRef $ initCost n
  U.forM_ rs $ \(i, j, k) -> do
    ms <- readSTRef mref
    writeSTRef mref (M.insert (i, j) k ms)
  forM_ [0 .. (n - 1)] $ \k -> do
    forM_ [0 .. (n - 1)] $ \i -> do
      forM_ [0 .. (n - 1)] $ \j -> do
        ms <- readSTRef mref
        let ov = fromJust $ M.lookup (i, j) ms
        let ikv = fromJust $ M.lookup (i, k) ms
        let kjv = fromJust $ M.lookup (k, j) ms
        let ikkjv = if ikv == maxBound || kjv == maxBound then maxBound else ikv + kjv
        writeSTRef mref (M.insert (i, j) (min ov ikkjv) ms)
  ms <- readSTRef mref
  return . sum $ M.elems ms
  where
    initCost :: Int -> M.HashMap (Int, Int) Int
    initCost n = M.fromList [((i, j), x) | i <- [0 .. (n -1)], j <- [0 .. (n -1)], let x = if i == j then 0 else maxBound]

main :: IO ()
main = do
  (n, m) <- readIntTuple <$> C.getLine
  vs <- U.replicateM m $ readIntTriple <$> C.getLine
  print $ solve n vs

readIntTuple :: C.ByteString -> (Int, Int)
readIntTuple b = (\v -> (v U.! 0, v U.! 1)) (U.unfoldrN 2 (runParser int) b)

readIntTriple :: C.ByteString -> (Int, Int, Int)
readIntTriple b = (\v -> (v U.! 0, v U.! 1, v U.! 2)) (U.unfoldrN 3 (runParser int) b)

type Parser a = StateT C.ByteString Maybe a

runParser :: Parser a -> C.ByteString -> Maybe (a, C.ByteString)
runParser = runStateT
{-# INLINE runParser #-}

int :: Parser Int
int = coerce $ C.readInt . C.dropWhile isSpace
{-# INLINE int #-}

int1 :: Parser Int
int1 = fmap (subtract 1) int
{-# INLINE int1 #-}

char :: Parser Char
char = coerce C.uncons
{-# INLINE char #-}

byte :: Parser Word8
byte = coerce B.uncons
{-# INLINE byte #-}

skipSpaces :: Parser ()
skipSpaces = modify' (C.dropWhile isSpace)
{-# INLINE skipSpaces #-}
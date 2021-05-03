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
import Data.Char (isSpace, ord)
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (foldl')
import Data.Maybe (fromJust, fromMaybe)
import Data.STRef (STRef)
import Data.STRef.Strict (modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)
import Debug.Trace

type Q = (Int, Int, Int)

pattern Q1 a b = (1, a, b)

pattern Q2 a b = (2, a, b)

pattern Q3 = (3, 0, 0)

pattern Q4 a b = (4, a, b)

{-# COMPLETE Q1, Q2, Q3, Q4 #-}

parseQ :: Parser Q
parseQ = do
  i <- int
  skipSpaces
  case i of
    3 -> do
      skipSpaces
      return (3, 0, 0)
    qi -> do
      a <- int1
      skipSpaces
      b <- int1
      skipSpaces
      return (qi, a, b)

solve :: Int -> U.Vector Q -> U.Vector Int
solve n qs = runST $ do
  rows <- U.thaw $ U.fromList [0 .. (n -1)]
  transposed <- newSTRef False
  columns <- U.thaw $ U.fromList [0 .. (n -1)]
  outputs <- UM.new . U.length $ U.filter (\(n, _, _) -> n == 4) qs
  oref <- newSTRef 0
  U.forM_ qs $ \case
    Q1 a b -> do
      tr <- readSTRef transposed
      if tr
        then do
          UM.swap columns a b
        else do
          UM.swap rows a b
    Q2 a b -> do
      tr <- readSTRef transposed
      if tr
        then do
          UM.swap rows a b
        else do
          UM.swap columns a b
    Q3 -> do
      modifySTRef' transposed not
    Q4 a b -> do
      tr <- readSTRef transposed
      r <-
        if tr
          then do
            UM.unsafeRead rows b
          else do
            UM.unsafeRead rows a
      c <-
        if tr
          then do
            UM.unsafeRead columns a
          else do
            UM.unsafeRead columns b
      o <- readSTRef oref
      UM.unsafeWrite outputs o ((r * n) + c)
      modifySTRef' oref (+ 1)
  U.freeze outputs

main :: IO ()
main = do
  n <- fst . fromJust . runParser int <$> C.getLine
  q <- fst . fromJust . runParser int <$> C.getLine
  qs <- U.unfoldrN q (runParser parseQ) <$> C.getContents
  U.mapM_ print $ solve n qs

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
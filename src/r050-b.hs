{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O2 #-}

import Control.Applicative
import Control.Arrow (second)
import Control.Monad.ST (runST)
import Control.Monad.State.Strict
import qualified Data.Bits as BT
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (isSpace)
import Data.Coerce (coerce)
import Data.List (foldl')
import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)
import Debug.Trace

solve :: Int -> Int -> Int -> Int -> Int
solve r b x y = solve' 0 (10 ^ 18 + 1) r b x y
  where
    solve' :: Int -> Int -> Int -> Int -> Int -> Int -> Int
    solve' mi ma r b x y
      | abs (ma - mi) <= 1 = mi
      | otherwise =
        let mid = (ma + mi) `div` 2
         in if
                | r < mid || b < mid -> solve' mi mid r b x y
                | ((r - mid) `div` (x - 1) + (b - mid) `div` (y - 1)) < mid -> solve' mi mid r b x y
                | otherwise -> solve' mid ma r b x y

main :: IO ()
main = do
  (r, b) <- (\v -> (v U.! 0, v U.! 1)) . U.unfoldrN 2 (runParser int) <$> C.getLine
  (x, y) <- (\v -> (v U.! 0, v U.! 1)) . U.unfoldrN 2 (runParser int) <$> C.getLine
  print $ solve r b x y

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
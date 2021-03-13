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

parseEw :: Parser Int
parseEw = do
  c <- char
  return (if c == 'E' then 1 else 0)

solve :: Int -> U.Vector Int -> Int
solve n ews =
  (\(z, _, _) -> z) $
    U.last $
      U.create $ do
        vs <- UM.new n
        let tailSum = U.sum $ U.tail ews
        UM.unsafeWrite vs 0 (tailSum, 0, tailSum)
        forM_ [1 .. (n -1)] $ \i -> do
          (total, left, right) <- UM.unsafeRead vs (i - 1)
          let pe = ews U.! (i - 1)
          let e = ews U.! i
          let nl = left + (1 - pe)
          let nr = right - e
          UM.unsafeWrite vs i (min (nl + nr) total, nl, nr)
        return vs

main :: IO ()
main = do
  n <- fst . fromJust . C.readInt <$> C.getLine
  ews <- U.unfoldrN n (runParser parseEw) <$> C.getLine
  print $ solve n ews

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
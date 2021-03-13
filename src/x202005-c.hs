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
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)
import Debug.Trace

solve :: Int -> Int -> Int -> Int -> Int
solve a r 0 mx = a
solve a r 1 mx = a
solve a r 2 mx = a * r
solve a r n mx =
  foldl'
    ( \ans i ->
        if ans > mx
          then ans
          else ans * r
    )
    a
    [0 .. (n - 2)]

main :: IO ()
main = do
  (a, r, n) <- (\v -> (v U.! 0, v U.! 1, v U.! 2)) . U.unfoldrN 3 (runParser int) <$> C.getLine
  let ans = solve a r n (10 ^ 9)
  putStrLn $ if ans > (10 ^ 9) then "large" else show ans

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
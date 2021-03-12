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
import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)
import Debug.Trace

solve :: Int -> U.Vector Int -> Int
solve n as =
  let ok = (2 ^ n)
   in (\vs -> vs U.! (ok - 1)) $
        U.create $ do
          cs <- UM.replicate (ok * n) maxBound
          UM.unsafeWrite cs 0 0
          forM_ [0 .. (ok - 1)] $ \path -> do
            forM_ [0 .. (n - 1)] $ \i -> do
              forM_ [0 .. (n - 1)] $ \j -> do
                when (i /= j && not (BT.testBit path j)) $ do
                  let a = as U.! (i * n + j)
                  ic <- UM.unsafeRead cs (i * ok + path)
                  when (ic /= maxBound) $ do
                    let jidx = j * ok + (path BT..|. BT.bit j)
                    jc <- UM.unsafeRead cs jidx
                    UM.unsafeWrite cs jidx (min jc $ a + (if ic == maxBound then 0 else ic))
          return cs

main :: IO ()
main = do
  n <- fst . fromJust . C.readInt <$> C.getLine
  as <- U.unfoldrN (n * n) (runParser int) <$> C.getContents
  print $ solve n as

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
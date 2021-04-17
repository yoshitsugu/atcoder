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
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Data.STRef.Strict (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)
import Debug.Trace

-- 壁ならFalse
parseB :: Parser Bool
parseB = do
  c <- char
  skipSpaces
  return $ c == '.'

main :: IO ()
main = do
  (h, w) <- readIntTuple <$> C.getLine
  vs <- U.unfoldr (runParser parseB) <$> C.getContents
  print $ solve h w vs

solve :: Int -> Int -> U.Vector Bool -> Int
solve h w vs =
  U.last $
    U.create $ do
      cs <- U.thaw $ U.replicate (h * w) 0
      UM.unsafeWrite cs 0 1
      forM_ [0 .. (h - 1)] $ \y -> do
        forM_ [0 .. (w - 1)] $ \x -> do
          let cv = vs U.! ((y * w) + x)
          cc <- UM.unsafeRead cs (y * w + x)
          when (cv && cc > 0) $ do
            when (x < w - 1) $ do
              let lv = vs U.! ((y * w) + (x + 1))
              when lv $ UM.unsafeModify cs ((`mod` (10 ^ 9 + 7)) . (+ cc)) (y * w + (x + 1))
            when (y < h - 1) $ do
              let bv = vs U.! (((y + 1) * w) + x)
              when bv $ UM.unsafeModify cs ((`mod` (10 ^ 9 + 7)) . (+ cc)) ((y + 1) * w + x)
      return cs

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
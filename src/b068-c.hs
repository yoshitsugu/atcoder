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
import qualified Data.HashMap.Strict as HS
import Data.List (foldl')
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)
import Debug.Trace

type Route = V.Vector (U.Vector Int)

main :: IO ()
main = do
  (n, m) <- (\v -> (v U.! 0, v U.! 1)) . U.unfoldrN 2 (runParser int) <$> C.getLine
  vs <- U.replicateM m $ (\v -> (v U.! 0, v U.! 1)) . U.unfoldrN 2 (runParser int) <$> C.getLine
  step1 <- UM.replicate (2 * 10 ^ 5 + 1) False :: IO (UM.IOVector Bool)
  step2 <- UM.replicate (2 * 10 ^ 5 + 1) False :: IO (UM.IOVector Bool)
  U.forM_ vs $ \(a, b) -> do
    when (a == 1) $ UM.write step1 b True
    when (b == n) $ UM.write step2 a True
  s1 <- U.unsafeFreeze step1
  s2 <- U.unsafeFreeze step2
  putStrLn $
    if U.or $ U.zipWith (&&) s1 s2
      then "POSSIBLE"
      else "IMPOSSIBLE"

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
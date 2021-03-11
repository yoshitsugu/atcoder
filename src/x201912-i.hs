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
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)
import Debug.Trace

parseYn :: Int -> Parser (Int, Int)
parseYn n = do
  yn <-
    foldM
      ( \r i ->
          do
            c <- char
            return $ r + (if c == 'Y' then 2 ^ i else 0)
      )
      0
      [0 .. n]
  c <- int
  skipSpaces
  return (yn, c)

solve :: Int -> Int -> U.Vector (Int, Int) -> Int
solve n l cs =
  let allY = 2 ^ n
   in (\v -> if v == maxBound then (-1) else v)
        . U.last
        $ U.create $
          do
            vs <- UM.replicate (allY * (l + 1)) maxBound
            UM.unsafeWrite vs 0 0
            forM_ [1 .. l] $ \ci -> do
              let c = cs U.! (ci - 1)
              forM_ [0 .. 2 ^ n] $ \yn -> do
                v <- UM.unsafeRead vs (allY * (ci - 1) + yn)
                when (v /= maxBound) $ do
                  pv <- UM.unsafeRead vs (allY * ci + yn)
                  ppv <- UM.unsafeRead vs (allY * ci + (yn BT..|. fst c))
                  UM.unsafeWrite vs (allY * ci + (yn BT..|. fst c)) (min ppv (v + snd c))
                  UM.unsafeWrite vs (allY * ci + yn) (min pv v)
            return vs

main :: IO ()
main = do
  (n, l) <- (\v -> (v U.! 0, v U.! 1)) . U.unfoldrN 2 (runParser int) <$> C.getLine
  cs <- U.unfoldrN l (runParser (parseYn n)) <$> C.getContents
  print $ solve n l cs

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
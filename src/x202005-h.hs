{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O2 #-}

import Control.Applicative
import Control.Arrow (second)
import Control.Monad (when)
import Control.Monad.ST (runST)
import Control.Monad.State.Strict
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (isSpace)
import Data.Coerce (coerce)
import qualified Data.List as L
import Data.Maybe (fromMaybe, mapMaybe)
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)
import Debug.Trace

solve :: Int -> Int -> Int -> Int -> U.Vector Int -> Int
solve l t1 t2 t3 xs =
  let hs = U.accumulate (+) (U.replicate (l + 2) 0) . U.zip xs $ U.replicate (U.length xs) t3
   in minimum . f $
        U.create $ do
          cs <- UM.new (l + 1)
          UM.unsafeWrite cs 0 0
          forM_
            [1 .. (l + 1)]
            ( \i -> do
                c1 <- UM.unsafeRead cs (i - 1)
                let c_1 = c1 + t1 + hs U.! i
                UM.unsafeWrite cs i c_1
                when
                  (i >= 2)
                  ( do
                      c2 <- UM.unsafeRead cs (i - 2)
                      let c_2 = min c_1 (c2 + t1 + t2 + hs U.! i)
                      UM.unsafeWrite cs i c_2
                      when
                        (i >= 4)
                        ( do
                            c3 <- UM.unsafeRead cs (i - 4)
                            let c_3 = min c_2 (c3 + t1 + 3 * t2 + hs U.! i)
                            UM.unsafeWrite cs i c_3
                        )
                  )
            )
          return cs
  where
    f :: U.Vector Int -> [Int]
    f cs =
      (cs U.! l) :
        [ (\li -> (cs U.! li) + (t1 `div` 2 + t2 * (2 * (l - li) - 1) `div` 2))
            li
          | li <- [l - 3, l - 2, l - 1],
            li >= 0
        ]

main :: IO ()
main = do
  (n, l) <- (\v -> (v U.! 0, v U.! 1)) . U.unfoldrN 2 (runParser int) <$> C.getLine
  xs <- U.unfoldrN n (runParser int) <$> C.getLine
  (t1, t2, t3) <- (\v -> (v U.! 0, v U.! 1, v U.! 2)) . U.unfoldrN 3 (runParser int) <$> C.getLine
  print $ solve l t1 t2 t3 xs

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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -O2 #-}

module Main where

import Control.Arrow (second)
import Control.Monad.ST (runST)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Debug.Trace (trace)

res :: Int -> Int
res n = res' 3 (True, False, False) n + res' 5 (False, True, False) n + res' 7 (False, False, True) n
  where
    res' :: Int -> (Bool, Bool, Bool) -> Int -> Int
    res' c (True, True, True) n
      | c <= n = 1 + res' (c * 10 + 3) (True, True, True) n + res' (c * 10 + 5) (True, True, True) n + res' (c * 10 + 7) (True, True, True) n
      | otherwise = 0
    res' c (t, f, s) n
      | c <= n = res' (c * 10 + 3) (True, f, s) n + res' (c * 10 + 5) (t, True, s) n + res' (c * 10 + 7) (t, f, True) n
      | otherwise = 0

main :: IO ()
main = do
  n <- fst . fromJust . B.readInt <$> B.getLine
  print $ res n

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -O2 #-}

import Control.Arrow (second)
import Control.Monad.ST (runST)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VM

sell :: Int -> VU.Vector Int -> VU.Vector (Int, Int, Int) -> Int
sell cn cs0 ss = runST $ do
  cs <- VU.thaw cs0
  (\(z, _, _, _, _) -> z)
    <$> VU.foldM'
      ( \(res, s, z, ms, mz) -> \case
          (1, x, a)
            | odd x -> do
              c <- VM.unsafeRead cs (x - 1)
              if (c - z - s) >= a
                then do
                  VM.unsafeWrite cs (x - 1) (c - a)
                  return (res + a, s, z, min ms (c - a), min mz (c - a))
                else return (res, s, z, ms, mz)
            | otherwise -> do
              c <- VM.unsafeRead cs (x - 1)
              if (c - z) >= a
                then do
                  VM.unsafeWrite cs (x - 1) (c - a)
                  return (res + a, s, z, ms, min mz (c - a))
                else return (res, s, z, ms, mz)
          (2, a, _)
            | ms >= a -> return (res + a * div (cn + 1) 2, s + a, z, ms - a, min mz (ms - a))
            | otherwise -> return (res, s, z, ms, mz)
          (3, a, _)
            | mz >= a -> return (res + a * cn, s, z + a, ms - a, mz - a)
            | otherwise -> return (res, s, z, ms, mz)
          _ -> error "invalid query"
      )
      (0, 0, 0, VU.minimum $ VU.ifilter (\i _ -> even i) cs0, VU.minimum cs0)
      ss

main :: IO ()
main = do
  let readInt = fmap (second B.tail) . B.readInt
      readInt' = fmap (\(i, r) -> if B.length r > 0 then (i, B.tail r) else (i, B.empty)) . B.readInt
  n <- fst . fromJust . B.readInt <$> B.getLine
  cs <- VU.unfoldrN n readInt <$> B.getLine
  q <- fst . fromJust . B.readInt <$> B.getLine
  ss <- VU.replicateM q $ (\vec -> (vec VU.! 0, vec VU.! 1, if VU.length vec == 3 then vec VU.! 2 else 0)) . VU.unfoldrN 3 readInt' <$> B.getLine
  print $ sell n cs ss

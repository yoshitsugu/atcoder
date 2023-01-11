{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O2 #-}

import Control.Applicative
import Control.Arrow (second)
import qualified Control.Monad as V
import Control.Monad.ST (runST)
import Control.Monad.State.Strict
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (isSpace)
import Data.Coerce (coerce)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)
import Debug.Trace

root :: Int -> IM.IntMap Int -> Int
root a m =
    case m IM.!? a of
        Just x -> if x == a then a else root x m
        Nothing -> a

isSame :: Int -> Int -> IM.IntMap Int -> Bool
isSame a b m = a == b || root a m == root b m

unite :: IM.IntMap Int -> IM.IntMap Int -> Int -> Int -> (Bool, IM.IntMap Int, IM.IntMap Int)
unite m s p c =
    let parentRoot = root p m
        childRoot = root c m
     in if parentRoot == childRoot
            then (False, m, s)
            else
                let ps = IM.findWithDefault parentRoot 0 s
                    cs = IM.findWithDefault childRoot 0 s
                    (pr, cr) = if ps < cs then (childRoot, parentRoot) else (parentRoot, childRoot)
                 in (True, IM.insert cr pr m, IM.insertWith (+) ps cr s)

main :: IO ()
main = do
    (n, m) <- (\v -> (v U.! 0, v U.! 1)) . U.unfoldrN 2 (runParser int) <$> C.getLine
    as <- V.replicateM m $ (\v -> ((v U.! 0) - 1, (v U.! 1) - 1)) . U.unfoldrN 2 (runParser int) <$> C.getLine
    let (rs, ss) = foldl' (\(rs, ss) (a, b) -> let (_, rs1, ss1) = unite rs ss a b in (rs1, ss1)) (IM.empty, IM.empty) as
    let (rrs, sss, cnt) = foldl' (\(rs, ss, cnt) i -> let (b, rs1, ss1) = unite rs ss 0 i in (rs1, ss1, if b then cnt + 1 else cnt)) (rs, ss, 0) [0 .. (n - 1)]
    print cnt

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

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

solve :: Int -> V.Vector (U.Vector (Int, Int)) -> Int
solve n rs = runST $ do
  mref <- newSTRef M.empty
  sref <- newSTRef $ S.singleton 0
  U.forM_ (rs V.! 0) $ \(b, c) -> do
    modifySTRef' mref $ M.insert (c, b) ()
  loop mref sref 1 n 0 rs
  where
    loop :: STRef s (M.Map (Int, Int) ()) -> STRef s (S.Set Int) -> Int -> Int -> Int -> V.Vector (U.Vector (Int, Int)) -> ST s Int
    loop mref sref scount n c rs
      | scount == n = return c
      | otherwise = do
        ms <- readSTRef mref
        ss <- readSTRef sref
        case M.minViewWithKey ms of
          Just (((nc, nt), ()), qs) -> do
            if S.member nt ss
              then do
                writeSTRef mref qs
                loop mref sref scount n c rs
              else do
                writeSTRef mref qs
                modifySTRef' sref (S.insert nt)
                U.forM_ (rs V.! nt) $ \(b, c) -> do
                  modifySTRef' mref $ M.insert (c, b) ()
                loop mref sref (scount + 1) n (nc + c) rs
          Nothing -> return c

main :: IO ()
main = do
  (n, m) <- readIntTuple <$> C.getLine
  vs <- U.replicateM m $ readIntTriple <$> C.getLine
  print . solve n $ toRoute n vs
  where
    toRoute :: Int -> U.Vector (Int, Int, Int) -> V.Vector (U.Vector (Int, Int))
    toRoute n vs = V.create $ do
      rs <- VM.replicate n U.empty
      U.forM_ vs $ \(a, b, c) -> do
        VM.unsafeModify rs (\v -> U.snoc v (b, c)) a
        VM.unsafeModify rs (\v -> U.snoc v (a, c)) b
      return rs

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
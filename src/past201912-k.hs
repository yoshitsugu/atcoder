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
import Data.Char (isSpace, ord)
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromJust, fromMaybe)
import Data.STRef (STRef)
import Data.STRef.Strict (modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)
import Debug.Trace

solve :: Int -> U.Vector Int -> Int -> U.Vector (Int, Int) -> U.Vector Bool
solve n ps q qs = runST $ do
  ss <- V.thaw $ V.replicate n U.empty
  root <- newSTRef 0
  U.zipWithM_
    ( \p i -> do
        if p < 0
          then do
            writeSTRef root i
          else do
            VM.unsafeModify ss (`U.snoc` i) (p - 1)
    )
    ps
    $ U.fromList [0 ..]
  root' <- readSTRef root
  ss' <- V.unsafeFreeze ss
  qs' <- V.thaw $ V.replicate n U.empty
  U.zipWithM_ (\(a, b) i -> VM.unsafeModify qs' (`U.snoc` (i, b - 1)) (a - 1)) qs $ U.fromList [0 ..]
  qs'' <- V.unsafeFreeze qs'
  boss <- U.thaw $ U.replicate n False
  ans <- U.thaw $ U.replicate q False
  UM.unsafeWrite boss root' True
  dfs root' ss' qs'' boss ans
  U.unsafeFreeze ans
  where
    dfs :: Int -> V.Vector (U.Vector Int) -> V.Vector (U.Vector (Int, Int)) -> UM.MVector s Bool -> UM.MVector s Bool -> ST s ()
    dfs i ss qs boss ans = do
      U.forM_ (qs V.! i) $ \(qi, b) -> do
        bs <- UM.unsafeRead boss b
        UM.unsafeWrite ans qi bs
      UM.unsafeWrite boss i True
      U.forM_ (ss V.! i) $ \s -> do
        dfs s ss qs boss ans
      UM.unsafeWrite boss i False

main :: IO ()
main = do
  n <- fst . fromJust . runParser int <$> C.getLine
  ps <- U.replicateM n $ fst . fromJust . runParser int <$> C.getLine
  q <- fst . fromJust . runParser int <$> C.getLine
  qs <- U.replicateM q $ readIntTuple <$> C.getLine
  U.forM_ (solve n ps q qs) $ \a ->
    if a
      then do
        putStrLn "Yes"
      else do
        putStrLn "No"

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
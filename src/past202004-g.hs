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
import Data.List (foldl')
import Data.Maybe (fromJust, fromMaybe)
import Data.STRef (STRef)
import Data.STRef.Strict (modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)
import Debug.Trace
import GHC.List (scanl')

type Q = (Int, Char, Int)

pattern Q1 c x = (1, c, x)

pattern Q2 d = (2, '.', d)

{-# COMPLETE Q1, Q2 #-}

parseQ :: Parser Q
parseQ = do
  i <- int
  case i of
    1 -> do
      skipSpaces
      c <- char
      x <- int
      skipSpaces
      return (1, c, x)
    2 -> do
      d <- int
      skipSpaces
      return (2, '.', d)
    _ -> error "Invalid query"

solve :: Int -> U.Vector Q -> U.Vector Int
solve n qs = runST $ do
  mss <- UM.new n
  msi <- newSTRef 0
  mse <- newSTRef 0
  U.forM qs $ \q -> do
    query q mss msi mse
  where
    query :: Q -> UM.MVector s (Char, Int) -> STRef s Int -> STRef s Int -> ST s Int
    query (Q1 c x) mss msi mse = do
      ei <- readSTRef mse
      UM.unsafeWrite mss ei (c, x)
      modifySTRef' mse (+ 1)
      return (-1)
    query (Q2 d) mss msi mse = do
      counts <- U.thaw $ U.replicate 26 0
      loop d mss msi mse counts
      fc <- U.unsafeFreeze counts
      return $ U.foldl' (\r v -> r + v ^ 2) 0 fc

    loop :: Int -> UM.MVector s (Char, Int) -> STRef s Int -> STRef s Int -> UM.MVector s Int -> ST s ()
    loop d mss msi mse counts = do
      si <- readSTRef msi
      se <- readSTRef mse
      if si >= se
        then return ()
        else do
          (hc, hx) <- UM.unsafeRead mss si
          if d < hx
            then do
              UM.unsafeModify counts (+ d) (ord hc - ord 'a')
              UM.unsafeWrite mss si (hc, hx - d)
            else do
              UM.unsafeModify counts (+ hx) (ord hc - ord 'a')
              when (si < se) $ do
                modifySTRef' msi (+ 1)
                loop (d - hx) mss msi mse counts

main :: IO ()
main = do
  n <- fst . fromJust . runParser int <$> C.getLine
  qs <- U.unfoldrN n (runParser parseQ) <$> C.getContents
  U.forM_ (solve n qs) $ \r -> do
    if r >= 0
      then do
        print r
      else do
        return ()

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
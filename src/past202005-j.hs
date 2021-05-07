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

solve :: Int -> Int -> U.Vector Int -> U.Vector Int
solve n m rs =
  U.create $ do
    tmp <- U.thaw $ U.replicate n (-1)
    ans <- U.thaw $ U.replicate m (-1)
    bs 0 (n - 1) rs 0 tmp ans
    return ans
  where
    bs :: Int -> Int -> U.Vector Int -> Int -> UM.MVector s Int -> UM.MVector s Int -> ST s ()
    bs i j rs ri tmp ans
      | U.length rs <= ri = return ()
      | otherwise = do
        let r = rs U.! ri
        let j' = (j - i) `div` 2 + i
        if j' < 0 || n <= j'
          then bs 0 (n - 1) rs (ri + 1) tmp ans
          else do
            tj' <- UM.unsafeRead tmp j'
            if r > tj'
              then do
                if abs (j' - i) < 2
                  then do
                    ti <- UM.unsafeRead tmp i
                    if r > ti
                      then do
                        UM.unsafeWrite tmp i r
                        UM.unsafeWrite ans ri (i + 1)
                      else do
                        UM.unsafeWrite tmp j' r
                        UM.unsafeWrite ans ri (j' + 1)
                    bs 0 (n - 1) rs (ri + 1) tmp ans
                  else bs i j' rs ri tmp ans
              else do
                if abs (j - j') < 2
                  then do
                    tj <- UM.unsafeRead tmp j
                    when (r > tj) $ do
                      UM.unsafeWrite tmp j r
                      UM.unsafeWrite ans ri (j + 1)
                    bs 0 (n - 1) rs (ri + 1) tmp ans
                  else bs j' j rs ri tmp ans

main :: IO ()
main = do
  (n, m) <- readIntTuple <$> C.getLine
  rs <- U.unfoldrN m (runParser int) <$> C.getLine
  U.mapM_ print (solve n m rs)

readIntTuple :: C.ByteString -> (Int, Int)
readIntTuple b = (\v -> (v U.! 0, v U.! 1)) (U.unfoldrN 2 (runParser int) b)

readInt1Tuple :: C.ByteString -> (Int, Int)
readInt1Tuple b = (\v -> (v U.! 0, v U.! 1)) (U.unfoldrN 2 (runParser int1) b)

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
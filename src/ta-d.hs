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
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe)
import Data.STRef.Strict (modifySTRef', newSTRef, readSTRef)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)
import Debug.Trace

solve :: Int -> V.Vector (U.Vector (Int, Int)) -> Int
solve n vs = runST $ do
  cs <- U.thaw . U.cons 0 $ U.replicate (n - 1) (maxBound :: Int)
  loop vs cs (M.singleton (0, 0) ())
  cs' <- U.freeze cs
  return $ U.last cs'
  where
    loop :: V.Vector (U.Vector (Int, Int)) -> UM.MVector s Int -> M.Map (Int, Int) () -> ST s ()
    loop vs cs q
      | Just (((qd, qf), _), q') <- M.minViewWithKey q = do
        refq <- newSTRef q'
        c <- UM.unsafeRead cs qf
        when (qd <= c) $ do
          U.forM_ (vs V.! qf) $ \(l', c') -> do
            lc' <- UM.unsafeRead cs l'
            when (c' + c < lc') $ do
              UM.unsafeWrite cs l' (c + c')
              modifySTRef' refq (M.insert (c + c', l') ())
        loop vs cs =<< readSTRef refq
      | otherwise = return ()

main :: IO ()
main = do
  (n, m) <- readIntTuple <$> C.getLine
  vs <- U.replicateM m $ readIntTriple <$> C.getLine
  print . solve n $ toRoute n vs
  where
    toRoute :: Int -> U.Vector (Int, Int, Int) -> V.Vector (U.Vector (Int, Int))
    toRoute n vs = do
      V.create $ do
        vs' <- V.thaw $ V.replicate n U.empty
        U.forM_ vs $ \(a, b, c) -> do
          VM.unsafeModify vs' (U.cons (b, c)) a
        return vs'

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
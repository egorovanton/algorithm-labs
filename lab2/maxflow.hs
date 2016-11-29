{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import Control.Monad
import Control.Monad.ST

import Data.Array.IO
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Unsafe
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C 
import Data.Maybe (fromJust)
import Data.STRef
import           Data.Sequence (Seq, ViewL((:<)), (><))
import qualified Data.Sequence as S
import Data.Foldable
import Data.Tuple

import System.IO


filename = "maxflow"

{-# INLINE readInts #-}
readInts input = do
   line <- C.hGetLine input
   return $! fst . fromJust . C.readInt <$> C.words line

main = do
    input <- openFile (filename ++ ".in") ReadMode

    [n, m]   <- readInts input
    capacity <- newArray ((1, 1), (n, n)) 0 :: IO (IOUArray (Int, Int) Int)
    forM_ [1..m] $ \_ -> do
        [i, j, w] <- readInts input
        writeArray capacity (i, j) w

    c <- unsafeFreeze capacity

    writeFile (filename ++ ".out") (show $ maxflow $ c)

maxflow :: UArray (Int, Int) Int -> Int
maxflow capacity = runST $ dinic capacity


bfs :: forall s. UArray (Int, Int) Int -> STUArray s (Int, Int) Int -> STUArray s Int Int -> ST s Bool
bfs capacity flow depth = do
        bnds <- getBounds depth
        forM_ (range bnds) $ \i -> writeArray depth i (-1)
        writeArray depth 1 0
        loop (S.singleton 1)
        (/= -1) <$> readArray depth n
    where
        (_, (_, n)) = bounds capacity
        loop :: Seq Int -> ST s ()
        loop queue | S.null queue = return ()
        loop queue = do
            let (v :< rest) = S.viewl queue
            next <- flip filterM [1..n] $ \to -> do
                        dv <- readArray depth v
                        d <- readArray depth to
                        f <- readArray flow (v, to)
                        let flag = d == -1 && f < capacity!(v, to)
                        when flag $! writeArray depth to (dv + 1)
                        return $! flag
            loop (rest >< S.fromList next)

dfs :: UArray (Int, Int) Int -> STUArray s (Int, Int) Int -> STUArray s Int Int -> STUArray s Int Int -> Int -> Int -> ST s Int
dfs capacity flow depth ptr from maxflow = let n = (snd.snd) (bounds capacity) in
    if (maxflow == 0 || from == n) then
        return maxflow
    else do
        answer <- newSTRef 0
        whileM ((&&)<$>((<=n) <$> ptr<!>from)<*>((==0)<$>readSTRef answer)) $ do
            to <- ptr<!>from
            dt <- readArray depth to
            df <- readArray depth from
            when (dt == df + 1) $ do
                pushed <- dfs capacity flow depth ptr to =<< (min maxflow <$> ((capacity!(from, to) -) <$> flow<!>(from, to)))
                x <- readArray ptr from
                modifyArray flow (from, to) (+ pushed)
                modifyArray flow (to, from) (\x-> x - pushed) 
                writeSTRef answer pushed 
            modifyArray ptr from (+ 1)
        readSTRef answer

dinic :: forall s . UArray (Int, Int) Int -> ST s Int
dinic capacity = do
    answer <- newSTRef 0    
    flow  <- newArray bnds 0
    depth <- newArray_ (1, n)
    ptr   <- newArray_ (1, n)
    whileM (bfs capacity flow depth) $ do
        d <- getElems depth
        f <- freeze flow :: ST s (UArray (Int, Int) Int)
        forM_ (range (1, n)) $ \i -> writeArray ptr i 1
        flag <- newSTRef True
        whileM (readSTRef flag) $ do
            pushed <- dfs capacity flow depth ptr 1 maxBound
            modifySTRef answer (+ pushed)
            writeSTRef flag (pushed > 0)
    readSTRef answer

    where
        bnds@(_, (_, n)) = bounds capacity


{-# INLINE (<!>) #-}
(<!>) :: (MArray a e m, Ix i) => a i e -> i -> m e
(<!>) = readArray

whileM p f = go
    where go = do
            x <- p
            if x then
                f >> go
            else 
                return ()

{-# INLINE modifyArray #-}
modifyArray :: (MArray a e m, Ix i, Monad m) => a i e -> i -> (e -> e) -> m ()
modifyArray arr i foo = readArray arr i >>= writeArray arr i . foo

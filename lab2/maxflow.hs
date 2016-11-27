{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE BangPatterns #-}

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

import System.CPUTime
-- import Debug.Trace

filename = "maxflow"


{-# INLINE readInts #-}
readInts input = do
   line <- C.hGetLine input
   return $! fst . fromJust . C.readInt <$> C.words line

main = do
    input <- openFile (filename ++ ".in") ReadMode

    [n, m]   <- readInts input
    capacity <- newArray ((1, 1), (n, n)) 0 :: IO (IOUArray (Int, Int) Int)
    graph    <- newArray (1, n) [] :: IO (IOArray Int [Int])
    forM_ [1..m] $ \_ -> do
        [i, j, w] <- readInts input
        writeArray capacity (i, j) w
        modifyArray graph i (j :)

    c <- unsafeFreeze capacity
    g <- unsafeFreeze graph
    -- print [n, m]
    -- print c
    -- print g
    writeFile (filename ++ ".out") (show $ maxflow c g)

maxflow :: UArray (Int, Int) Int -> Array Int [Int] -> Int
maxflow capacity graph = runST $ helper capacity graph =<< newArray (bounds capacity) 0

helper :: forall s. UArray (Int, Int) Int -> Array Int [Int] -> STUArray s (Int, Int) Int -> ST s Int
helper capacity graph flow = do
    parent <- newArray (bounds graph) (- 1)
    max_flow <- newSTRef 0
    whileM (findPath capacity graph flow parent) $ do
        preds <- getPreds parent n
        let indexes = zip (tail preds) preds
        inc <- foldrM (\x acc -> min acc <$> fmap (capacity!x -) (flow<!>x)) maxBound indexes
        forM_ indexes $ \i -> do
            modifyArray flow i (+ inc)
            modifyArray flow (swap i) (\x -> x - inc)
        modifySTRef max_flow (+ inc)
    readSTRef max_flow
    where
        getPreds :: STUArray s Int Int -> Int -> ST s [Int]
        getPreds parent (-1) = return []
        getPreds parent i    = (i :) <$> (getPreds parent =<< to)
            where to = readArray parent i 
        
        (_, n) = bounds graph

(<!>) :: (MArray a e m, Ix i) => a i e -> i -> m e
(<!>) = readArray

modifyArray :: (MArray a e m, Ix i, Monad m) => a i e -> i -> (e -> e) -> m ()
modifyArray arr i foo = readArray arr i >>= writeArray arr i . foo

-- bfs
findPath :: forall s. UArray (Int, Int) Int -> Array Int [Int] -> STUArray s (Int, Int) Int -> STUArray s Int Int -> ST s Bool
findPath capacity graph flow parent = do
        color <- newArray (a, n) white
        loop color (S.singleton a)
        (black == ) <$> color<!>n
    where
        loop :: STUArray s Int Int -> Seq Int -> ST s ()
        loop color !queue
            | S.null queue = return ()
            | otherwise    = do
                let (i :< rest) = S.viewl queue
                writeArray color i black
                nexts <- filterM (\j -> (&&) <$> ((white ==) <$> color<!>j) <*> ((>0).(capacity!(i,j) -) <$> flow<!>(i,j))) (graph!i)
                forM_ nexts $ \j -> do
                    writeArray color j grey
                    writeArray parent j i
                loop color $ rest >< S.fromList nexts

        ((a, b), (n, m)) = bounds capacity
        white = 0
        grey  = 1
        black = 2

whileM cond action = do
    c <- cond
    when c $ do
        action
        whileM cond action
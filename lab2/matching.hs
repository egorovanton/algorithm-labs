module Main where

import Control.Monad
import Control.Monad.ST
-- import Control.Monad.Reader
-- import Control.Monad.Trans.Maybe
import Data.Array.IO
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Unsafe
import Data.STRef
import System.IO

--import Debug.--Trace

filename = "matching"
readInts handle = map read . words <$> hGetLine handle

main = do
    input <- openFile (filename ++ ".in") ReadMode

    [n, m, k] <- readInts input
    graph <- newArray ((1, 1), (n, m)) False :: IO (IOUArray (Int, Int) Bool)
    forM_ [1..k] $ \_ -> do
        [i, j] <- readInts input
        writeArray graph (i, j) True

    g <- unsafeFreeze graph
    --print g
    writeFile (filename ++ ".out") (show $ maxMatch g)

maxMatch = length . filter id . helper

helper :: UArray (Int, Int) Bool -> [Bool]
helper graph = runST $ do
    let ((a, b), (n, m)) = bounds graph
    match <- newArray (b, m) 0
    forM [a..n] $ \i -> do
        empty <- newArray (b, m) False
        bfs graph match empty i

bfs :: UArray (Int, Int) Bool -> STUArray s Int Int -> STUArray s Int Bool -> Int -> ST s Bool 
bfs graph match visited i = do
    let ((a, b), (n, m)) = bounds graph
    var <- newSTRef b
    flag <- newSTRef False

    --trace "Hello" (return ())
    --trace ("i = " ++ show i) (return ())
    while (liftM2 (&&) ((<= m) <$> readSTRef var) (not <$> readSTRef flag)) $ do
        j <- readSTRef var
        v <- readArray visited j
        --trace ("j = " ++ show j) (return ())
        when (graph!(i, j) && not v) $ do
            writeArray visited j True
            ma <- readArray match j
            --trace ("ma = " ++ show ma) (return ())
            if ma == 0 then do
                writeArray match j i
                writeSTRef flag True
            else
                whenever (bfs graph match visited ma) $ do
                    writeArray match j i
                    writeSTRef flag True
        modifySTRef' var (+ 1)
    readSTRef flag


whenever cond action = do
    c <- cond
    when c action

while cond action = do
    c <- cond
    when c $ do
        action
        while cond action
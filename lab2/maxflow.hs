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
        modifyArray graph j (negate i :)

    c <- unsafeFreeze capacity
    g <- unsafeFreeze graph

    writeFile (filename ++ ".out") (show $ maxflow c g)

maxflow :: UArray (Int, Int) Int -> Array Int [Int] -> Int
maxflow capacity graph = runST $ helper capacity graph =<< newArray (bounds capacity) 0

helper :: forall s. UArray (Int, Int) Int -> Array Int [Int] -> STUArray s (Int, Int) Int -> ST s Int
helper capacity graph flow = do
    p <- findPath capacity graph flow
    case p of 
        Nothing          -> return 0
        Just ((m, path)) -> do
            mapM_ (\(i, j) -> change i j m) $ zip path (tail path)
            (m +) <$> helper capacity graph flow
    where
        change :: Int -> Int -> Int -> ST s ()
        change i j w 
            | capacity!(i, j) /= 0 = modifyArray flow (i, j) (+ w)
            | otherwise            = modifyArray flow (j, i) (\x -> x - w)

modifyArray :: (MArray a e m, Ix i, Monad m) => a i e -> i -> (e -> e) -> m ()
modifyArray arr i foo = readArray arr i >>= writeArray arr i . foo

--почему не компилится если завести runST?
findPath :: forall s. UArray (Int, Int) Int -> Array Int [Int] -> STUArray s (Int, Int) Int -> ST s (Maybe (Int, [Int]))
findPath capacity graph flow = do
    visited <- newArray (bounds graph) False
    findPath' visited 1
    where 
        findPath' :: STUArray s Int Bool -> Int -> ST s (Maybe (Int, [Int]))
        findPath' visited i
            | i == n    = return $! Just (maxBound, [n])
            | otherwise = loop (graph!i)
            where 
                ((a, b), (n, k)) = bounds capacity

                loop :: [Int] -> ST s (Maybe (Int, [Int]))
                loop []     = return Nothing
                loop (x:xs) = do
                    let j = abs x
                    f1 <- readArray flow (i, j)
                    f2 <- readArray flow (j, i)
                    let m = if x > 0 then capacity!(i, j) - f1 else f2
                    v <- readArray visited j
                    if m == 0 || v then
                        loop xs
                    else do
                        writeArray visited i True
                        p <- findPath' visited j
                        case p of
                            Nothing         -> loop xs
                            Just (m1, path) -> return $! Just (min m m1, i:path)
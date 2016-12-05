{-# OPTIONS_GHC -O2 #-}
  
module Main where
  
import Control.Monad
import Control.Monad.ST
import Data.Array.IO
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Unsafe
import Data.STRef
import System.IO

-- import qualified Data.ByteString.Char8 as C 
  
 
filename = "paths"
  
-- {-# INLINE readInts #-}
-- readInts input = do
--    line <- C.hGetLine input
--    let Just (a, s) = C.readInt line
--        Just (b, _) = C.readInt $! C.tail s
--    return $! (a, b)
  
readInts input = map read . words <$> hGetLine input

main = do
    input <- openFile (filename ++ ".in") ReadMode
  
    [n, m] <- readInts input
    bigraph <- newArray (1, n) [] :: IO (IOArray Int [Int])
    forM_ [1..m] $ \_ -> do
        [i, j] <- readInts input
        modifyArray bigraph i (j :)  

    g <- unsafeFreeze bigraph
    writeFile (filename ++ ".out") (show $! n - maxMatch g)
  
  
maxMatch :: Array Int [Int] -> Int
maxMatch graph = runST $ do
    let bnds = bounds graph
    match <- newArray bnds 0
    empty <- newArray bnds False
    count <- newSTRef 0
    forM_ (range bnds) $ \i -> do
        forM_ (range bnds) $ \j -> writeArray empty j False
        whenever (bfs graph match empty i) $! modifySTRef' count (+ 1)
    readSTRef count
  
  
bfs :: Array Int [Int] -> STUArray s Int Int -> STUArray s Int Bool -> Int -> ST s Bool 
bfs graph match visited i = loop (graph!i)
    where
        loop [] = return False
        loop (j:js) = do
            v <- readArray visited j
            if not v then do
                writeArray visited j True
                ma <- readArray match j
                if ma == 0 then
                    writeArray match j i >> return True
                else do
                    b <- bfs graph match visited ma
                    if b then
                        writeArray match j i >> return True
                    else
                        loop js
            else loop js

{-# INLINE whenever #-}
whenever cond action = do
    c <- cond
    when c action
  
while :: (Monad m) => m Bool -> m a -> m ()
while p f = go
    where go = do
            x <- p
            if x
                then f >> go
                else return ()

modifyArray :: (MArray a e m, Ix i, Monad m) => a i e -> i -> (e -> e) -> m ()
modifyArray arr i foo = do
    x <- readArray arr i 
    let x' = foo x
    x' `seq` writeArray arr i x'

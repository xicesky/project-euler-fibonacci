
module Fibonacci.Fibonacci where

import Debug.Trace (trace)

import GHC.Natural
import qualified Data.MemoCombinators as Memo
import Data.Bits ((.&.))
import Control.Exception (assert)

-- Very slow - like O(2^n) - dont use this
fibSimple :: Natural -> Natural
fibSimple n = --trace ("fibSimple " ++ show n) $
    fib n where
        fib 0 = 0
        fib 1 = 1
        fib n = fib (n-1) + fib (n-2)

-- Somewhat decent with Memo, O(n*n_op), big mem
fibMemo :: Natural -> Natural
fibMemo = Memo.integral $
    \n -> -- trace ("fibMemo " ++ show n) $
    fib' n where
        fib' :: Natural -> Natural
        fib' 0 = 0
        fib' 1 = 1
        fib' n = fibMemo (n-1) + fibMemo (n-2)

-- Somewhat fast, O(log(n)*n_op) but lots of memory for memo
fibFast :: Natural -> Natural
fibFast = Memo.integral $
    \n -> -- trace ("fibFast " ++ show n) $
    fib' n where
        fib' :: Natural -> Natural
        fib n | n > 100000 = error $ "fib " ++ show n
        fib' 0 = 0
        fib' 1 = 1
        fib' n | even n = let
            m = n `div` 2
            fm = fibFast m
            in (2 * fibFast (m-1) + fm) * fm
        fib' n = let
            m = (n+1) `div` 2
            in fibFast m ^ 2 + fibFast (m-1) ^ 2

-- The next fib works like this accumulating "fromBits" function
-- Convert MSB bit representation to Natural
fromBits :: [Int] -> Natural
fromBits = fbit 0 where
    fbit :: Natural -> [Int] -> Natural
    fbit n []     = n
    fbit n (0:xs) = fbit (2*n  ) xs
    fbit n (1:xs) = fbit (2*n+1) xs

-- Convert Natural to MSB bit representation
toBits :: Natural -> [Int]
toBits = toBits' [] where
    toBits' :: [Int] -> Natural -> [Int]
    toBits' xs 0            = xs
    toBits' xs n | even n   = toBits' (0 : xs) (n `div` 2)
                 | odd n    = toBits' (1 : xs) ((n-1) `div` 2)

-- Nice and O(log(n)*n_op), with almost no memory
{- The performance is dominated by operations on large integers
    in doubleFib
-}
fibFaster :: Natural -> Natural
fibFaster n = -- trace ("fibFaster " ++ show n) $
    recursive 0 (1, 0) . toBits $ n where
    plusOneFib :: (Natural, Natural) -> (Natural, Natural)
    plusOneFib (fn1, fn0) = (fn0, fn0 + fn1)
    
    doubleFib :: (Natural, Natural) -> (Natural, Natural)
    doubleFib (fn1, fn0) = (fn0^2+fn1^2, (2*fn1+fn0)*fn0)
    
    fib :: Natural -> (Natural, Natural) -> [Int] -> Natural
    fib n (fn1, fn0) []             = fn0
    fib n fnpair     (0:xs)         = recursive (2*n  ) (doubleFib fnpair) xs
    fib n fnpair     (1:xs)         = recursive (2*n+1) (plusOneFib $ doubleFib fnpair) xs

    recursive :: Natural -> (Natural, Natural) -> [Int] -> Natural
    recursive n fnpair r = -- trace ("fib " ++ show n ++ " " ++ show fnpair ++ " " ++ show r) $
        fib n fnpair r

-- fibFaster is our default implementation
fib :: Natural -> Natural
fib = fibFaster


-- Find first n so that fib n > max
-- "Stupid" version, just repeats calls to fib
findFibGreater :: Natural -> Natural
findFibGreater max = recursive 0 where
    recursive :: Natural -> Natural
    recursive = find
    find :: Natural -> Natural
    find n = if fib n > max
        then n
        else recursive (n+1)

{- Sum all even fibonacci numbers fib [n0, nmax]
    extremely stupid version that calculates all the fibs
    and checks if they are even
-}
fibSumStupid :: Natural -> Natural -> Natural
fibSumStupid n0 nmax = foldr1 (+)
    [f  | n <- [n0..nmax]
        , let f = fib n
        , even f
    ]

{- Sum all even fibonacci numbers fib [0, nmax]
    since fib(n) is even when n mod 3 = 0, just
    sum every third fibonacci number
-}
fibSumSmart :: Natural -> Natural
fibSumSmart nmax = foldr1 (+)
    [assert (even f) f
        | n <- [0,3..nmax]
        , let f = fib n
    ]

{- Sum all even fibonacci numbers
        f := fib n
        with f <= fmax
-}
fibSumMax :: Natural -> Natural
fibSumMax fmax = foldr1 (+) $
    takeWhile (<= fmax)
    [assert (even f) f
        | n <- [0,3..]
        , let f = fib n
    ]

{- TODO: And finally the even smarter solution:
    fib(3n) = 5*fib(n)^3 + 3 (-1)^n fib(n)
-}

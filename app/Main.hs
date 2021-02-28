
module Main where

import Criterion.Main (Benchmark, bench, bgroup, whnf)
import qualified Criterion.Main as Criterion

import GHC.Natural
import Fibonacci.Fibonacci

fibFuns :: [([Char], Natural, Natural -> Natural)]
fibFuns =
    [ ("fibSimple",      30,  fibSimple)
    , ("fibMemo",      1000,  fibMemo  )
    , ("fibFast",    100000,  fibFast  )
    , ("fibFaster", 1000000,  fibFaster)
    ]

runBench n = bgroup ("fib" ++ show n)
    [   bench name $ whnf fn n
    |   (name, max, fn) <- fibFuns
    ,   n <= max
    ]

benchmarks :: [Benchmark]
benchmarks = [runBench 25, runBench 1000]

showFun :: (Show a, Show b) => String -> (a -> b) -> a -> IO ()
showFun name f n =
    putStrLn $ name ++ "(" ++ show n ++ ") = " ++ show (f n)

demo :: IO ()
demo = do
    -- showFun "fibFaster" fibFaster (10^8)
    showFun "findFibGreater" findFibGreater (4*10^6)
    showFun "fibSumStupid" (uncurry fibSumStupid) (0, 10)
    showFun "fibSumStupid" (uncurry fibSumStupid) (0, 33)   -- This actually answers the problem
    showFun "fibSumSmart" fibSumSmart 33
    showFun "fibSumMax" fibSumMax (4*10^6)

main :: IO ()
main = do
    --demo
    Criterion.defaultMain benchmarks

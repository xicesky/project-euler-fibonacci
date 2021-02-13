
module Main where

import GHC.Natural
import Fibonacci.Fibonacci

showFun :: (Show a, Show b) => String -> (a -> b) -> a -> IO ()
showFun name f n =
    putStrLn $ name ++ "(" ++ show n ++ ") = " ++ show (f n)



main :: IO ()
main = do
    -- showFun "fibFaster" fibFaster (10^8)
    showFun "findFibGreater" findFibGreater (4*10^6)
    showFun "fibSumStupid" (uncurry fibSumStupid) (0, 10)
    showFun "fibSumStupid" (uncurry fibSumStupid) (0, 33)   -- This actually answers the problem
    showFun "fibSumSmart" fibSumSmart 33
    showFun "fibSumMax" fibSumMax (4*10^6)

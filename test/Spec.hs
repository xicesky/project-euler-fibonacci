
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-} 

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import GHC.Natural
import Fibonacci.Fibonacci

import Debug.Trace

-- Shortcut, we don't want to specify types all the time
toNat :: Integral a => a -> Natural
toNat = fromIntegral

-- Helper generator for "large" natural numbers
expNat :: Int -> Gen Natural
expNat e = do
    i <- (choose (e, e+1) :: Gen Int) -- exponent
    j <- (choose (0, 1000) :: Gen Int) -- offset
    return $ 10 ^ toNat i + toNat j

-- Check fromBits . toBits
spec_fromBitsInvToBits :: Spec
spec_fromBitsInvToBits = prop "fromBits . toBits == id" $
    forAll (expNat 100) $ \n -> n == (fromBits . toBits) n

-- Function has the fibonacci property
prop_Fibonacci :: (Eq a, Num a) => (a -> a) -> a -> Bool
prop_Fibonacci f n = f n == f (n-1) + f (n-2)

-- Function calculates fibonacci numbers
spec_isFib :: Int -> (Natural -> Natural) -> Spec
spec_isFib nmax f = do
    prop "has f(0)=0" $ f 0 == 0
    prop "has f(1)=1" $ f 1 == 1
    prop "has the fibonacci property for n >= 2" $
        forAll (choose (2, nmax) :: Gen Int) $ (. fromIntegral) $
            \i -> (i > 2) ==> prop_Fibonacci f i

-- Shows off how "fast" our fib function is on large numbers
spec_isFastFib :: (Natural -> Natural) -> Spec
spec_isFastFib f = prop "has a really fast fib impl" $    -- TODO: actually measure speed?
    forAll (expNat 6) $ prop_Fibonacci f

main :: IO ()
main = hspec $ do
    describe "Fibonacci.Fibonacci" $ do
        spec_fromBitsInvToBits
        describe "fibSimple" $ spec_isFib 30 fibSimple      -- anything >30 is unbelievably slow
        describe "fibMemo" $ spec_isFib 10000 fibMemo
        describe "fibFast" $ spec_isFib 1000000 fibFast
        describe "fibFaster" $ spec_isFib 1000000 fibFaster
        spec_isFastFib fibFaster

{-# LANGUAGE FlexibleInstances #-}

module CV.Test where

import Test.QuickCheck
import CV
import System.IO.Unsafe (unsafePerformIO)

instance Arbitrary (RGBT U8) where
  arbitrary = do
    r <- arbitrary
    g <- arbitrary
    b <- arbitrary
    return (RGBT r g b)


prop_eqToSelf :: RGBT U8 -> Property
prop_eqToSelf a =
  forAll (do
        h <- choose (1,1000)
        w <- choose (1,1000)
        return (h,w)) $ \(h,w) -> monoColor h w a == monoColor h w a

prop_monoColorAdd :: RGBT U8 -> RGBT U8 -> Property
prop_monoColorAdd a@(RGBT r1 g1 b1) b@(RGBT r2 g2 b2) = 
  forAll (do
        h <- choose (1,1000)
        w <- choose (1,1000)
        return (h,w)) $ \(h,w) -> validSum r1 r2 &&  validSum g1 g2 &&  validSum b1 b2 ==> monoColor h w a +:+ monoColor h w b == monoColor h w (addColor a b)



{-# LANGUAGE FlexibleInstances #-}

module CV.Test where

import Test.QuickCheck
import CV
import System.IO.Unsafe (unsafePerformIO)

instance Arbitrary (MatT U8 C1) where
  arbitrary = do
   -- rows <- choose (1000,1000)
  --  cols <- choose (1000,1000)
    return $ unsafePerformIO $ (randMat (1000 ::Int) (1000 ::Int):: IO (MatT U8 C1))   -- Is this okay?

instance Arbitrary (RGBT U8) where
  arbitrary = do
    r <- arbitrary
    g <- arbitrary
    b <- arbitrary
    return (RGBT r g b)

prop_commute :: (DepthType a, ChannelType b) => (MatT a b) -> (MatT a b) -> Bool
prop_commute a b = (a +:+ b) == (b +:+ a) && (a *:* b) == (b *:* a)


prop_monoColorAdd :: Int -> Int -> RGBT U8 -> RGBT U8 -> Bool
prop_monoColorAdd h w a b = monoColor h w a +:+ monoColor h w b == monoColor h w (addColor a b)



module Main where

import Test.QuickCheck

import CV
import CV.Test

main = quickCheck (prop_commute :: (MatT U8 C1) -> (MatT U8 C1) -> Bool)


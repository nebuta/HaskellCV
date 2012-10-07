module Main where

import Test.QuickCheck

import CV
import CV.Test

main = do
  quickCheckWith stdArgs{maxDiscard=1000} prop_eqToSelf
  quickCheckWith stdArgs{maxDiscard=1000} prop_monoColorAdd

module Main where

import Test.QuickCheck

import CV
import CV.Test

main = quickCheck prop_monoColorAdd

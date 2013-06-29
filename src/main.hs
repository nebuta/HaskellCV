-- main.hs

{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C -- get the C type
import CV.Core


-- RGB values with blue varied
mycolors :: [RGB]
mycolors = map (RGB 0 255) [0,50..255]

main = do
  img <- readImg "test.jpg"
  print (matId img)
  showMat img
  showMat (gauss 3 img)
  showMat (cvtColor rgbToGray img)
  return ()

showColors = do
  let a = randMat 50 50
  let r = monoColor (Channel B32) 50 50 red
  let g = monoColor (Channel B32) 50 50 (RGB 0 200 0)
  let ms = map (monoColor (Channel B32) 100 100) mycolors
  mapM_ showMat ms
--  print (matId a)
--  c_matTest
--  showMat (r +:+ g)
  return ()

test = do
  print (randMat 50 50)
  print (zeros 2)
  print (zeros 3)
  putStrLn (show (matsize (Mat 2)))

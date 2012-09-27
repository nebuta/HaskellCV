module CV.Demo where

import CV.Core
import CV.Filter

import Data.List (foldl')

demo1 :: IO ()
demo1 = do
  let imgs = map (monoColor (Channel B16) 300 300) [yellow,blue,red]
  mapM_ showMat imgs


demo2 :: IO ()
demo2 = do
  img <- readImg "test.jpg"
  let gray = fromImg img :: GrayImage     -- This is so cool! Automatic image conversion by polymorphic type.
  let res = map (\s -> (apply (gauss s)) gray) [3,5,7,9]
  mapM_ showImg res

demo3 = do
  img <- readImg "test.jpg"
  let gray = fromImg img :: GrayImage     -- This is so cool! Automatic image conversion by polymorphic type.
  let res = foldl' (\img s -> (apply (gauss s)) img) gray (replicate 10000 3)
  showImg res

demo4 = do
  img <- readImg "test.jpg"
  mapM_ (showImg . flip apply img) [gauss 3, medianFilter 3,medianFilter 5, medianFilter 7,laplacian 3 1 0, bilateral 3 5 5]

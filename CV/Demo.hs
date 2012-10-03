module CV.Demo where

import CV.Core
import CV.Filter

import Data.List (foldl')

demo1 :: IO ()
demo1 = do
  let imgs = map (monoColor Ch1 300 300) [yellow,blue,red]
  mapM_ showMatT imgs


demo2 :: IO ()
demo2 = do
  img <- readImg "test.jpg"
  let gray = cvtColor rgbToGray img
  let res = map (\s -> (gauss s gray)) [3,5,7,9]
  mapM_ showMatT res

demo3 = do
  img <- readImg "test.jpg"
  let gray = cvtColor rgbToGray img
  let res = (iterate (gauss 3) gray) !! 10000
  showMatT res

demo4 = do
  img <- readImg "test.jpg"
  mapM_ (showMatT . ($ img)) [gauss 3, medianFilter 3,medianFilter 5, medianFilter 7,laplacian 3 1 0, bilateral 3 5 5]
  let gray = cvtColor rgbToGray img 
  showMatT $ (dilate (fromStrEl (Ellipse 15 15))) gray

demo5 = do
  img <- readImg "test.jpg"
  let cmp = invariant (dilate disc . gauss 5) img
  showMatT img
  showMatT (cmp /: 2 *:* img /: 2)
    where disc = fromStrEl (Ellipse 5 5)

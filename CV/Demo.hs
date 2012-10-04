module CV.Demo where

import CV.Core
import CV.Filter

import Data.List (foldl')

demo1 :: IO ()
demo1 = do
  let imgs = map (monoColor 300 300) [yellow,blue,red]
  mapM_ showMatT imgs


demo2 :: IO ()
demo2 = do
  img <- readImg "test.jpg"
  let gray = convert img :: GrayImage
  let res = map (\s -> (gauss s gray)) [3,5,7,9]
  mapM_ showMatT res

demo3 = do
  img <- readImg "test.jpg"
  let gray = convert img :: GrayImage
  let res = (iterate (gauss 3) gray) !! 10000
  showMatT res

demo4 = do
  img <- readImg "test.jpg"
  mapM_ (showMatT . ($ img)) [gauss 3, medianFilter 3,medianFilter 5, medianFilter 7,laplacian 3 1 0, bilateral 3 5 5]
  let gray = convert img :: GrayImage 
  showMatT $ (dilate (fromStrEl (Ellipse 15 15))) gray

demo5 = do
  img <- fmap convert $ readImg "test.jpg"
  let cmp = invariant (dilate disc . gauss 5) img :: MatT U8 C1 Gray
  showMatT img
  showMatT (cmp /: 2 *:* img /: 2)
    where disc = fromStrEl (Ellipse 5 5)

demo6 = do
  img <- fmap convert $ readImg "test.jpg"
  let cmp = invariant (dilate disc . gauss 5) img :: MatT U8 C1 Gray
  print (sum (map sum (pixels cmp)))
      where disc = fromStrEl (Ellipse 5 5)

demo7 = do
  img <- fmap convert $ readImg "test.jpg"
  let cmp = invariant (dilate disc . gauss 5) img :: MatT U8 C1 Gray
  print (const "hi" (pixels cmp))     --cmp is not calculated because of lazy evaluation.
  return ()
      where disc = fromStrEl (Ellipse 5 5)


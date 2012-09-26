module CV.Demo where

import CV.Core
import CV.Filter

demo1 :: IO ()
demo1 = do
  let imgs = map (monoColor (Channel B16) 300 300) [yellow,blue,red]
  mapM_ showMat imgs


demo2 :: IO ()
demo2 = do
  img <- readImg "test.jpg"
  let gray = fromImg img :: GrayImage     -- This is so cool! Automatic image conversion by polymorphic type.
  let imgs = map (\x -> (apply (gauss x)) gray) [3,5,7,9]
  mapM_ showImg imgs


module Main where

import CV
import CV.Filter
import CV.Demo

import Data.List (sort)
import Debug.Trace (trace,traceShow)

import Control.Concurrent (forkIO)

findParticles :: GrayImage -> [Pos]
findParticles = detect . prefilter
  where prefilter = gauss 3

detect :: GrayImage -> [Pos]
detect img = refinePos $ filtered maxima
  where
    maxima = findIndexMat cmpEqual img dilated
    filtered ms = filterThresAndDist minInt minDist img ms
    perc = 20.0   -- in %
    minInt = fromIntegral $ percentile perc img
    minDist = 3
    dilated = dilate (fromStrEl (Ellipse 3 3)) img
    intensities = (reverse . sort . concat . pixels) img

findIndexMat :: CmpFun a b -> MatT a b -> MatT a b -> [Coord]
findIndexMat f a b = findNonZero $ CV.compare f a b

refinePos :: [Coord] -> [Pos]
refinePos cs = map f cs
  where f (Coord y x) = Pos (fromIntegral y) (fromIntegral x)--Stub

filterThresAndDist :: Double -> Double -> MatT U8 C1-> [Coord] -> [Coord]
filterThresAndDist int dist mat ps = filter (\p -> f int mat p && g ps dist p) ps
  where
    f :: Double -> MatT U8 C1-> Coord -> Bool
    f int mat (Coord y x) = (fromIntegral $ pixelAt y x mat) >= int
--    f _ _ _ = True 
    g :: [Coord] -> Double -> Coord -> Bool
    g ps dist p = all (\x -> x < 0.1 || x>d2) $ map (distSq p) ps -- not very clean. Comparison with all non-self is the best way.
    d2 = dist * dist
    distSq :: Coord -> Coord -> Double
    distSq (Coord y1 x1) (Coord y2 x2) = fromIntegral ( (y1-y2)*(y1-y2) + (x1-x2)*(x1-x2) )


data Traj = Traj [Pos3D]

connectTraj :: [[Pos3D]] -> [Traj]
connectTraj ps = [Traj []] --stub

getTraj :: [GrayImage] -> [Traj]
getTraj imgs = connectTraj . addFrameIdx . map findParticles $ imgs

addFrameIdx :: [[Pos]] -> [[Pos3D]]
addFrameIdx pss = zipWith f [0..length pss-1] pss
  where
    f i ps = map (g i) ps
    g i (Pos x y) = Pos3D i x y

main = mapM_ maintrue [1..100]

maintrue :: Int -> IO ()
maintrue n = do
  img <- readImg "cell.jpg"
  let ps = findParticles (convert img)
  print (n,(length ps))
  return ()

demos :: IO ()
demos = do
  demo8

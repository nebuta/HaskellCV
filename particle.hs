module Main where

import CV.Core
import CV.Filter
import CV.Demo

import Data.List (sort)
import Debug.Trace (trace)

findParticles :: GrayImage -> [Pos]
findParticles = detect . prefilter
  where prefilter = gauss 3

detect :: GrayImage -> [Pos]
detect img = refinePos $ filterThresAndDist minInt minDist img $ findIndexMat cmpEqual img dilated
  where
    percentile = 10.0   -- in %
    minInt = fromIntegral $ intensities !! (floor $ percentile / 100.0 * fromIntegral (length intensities))
    minDist = 1
    dilated = dilate (fromStrEl (Ellipse 3 3)) img
    intensities = (reverse . sort . concat . pixels) img

findIndexMat :: CmpFun a b c -> MatT a b c -> MatT a b c-> [Coord]
findIndexMat f a b = findNonZero $ CV.Core.compare f a b

refinePos :: [Coord] -> [Pos]
refinePos cs = map f cs
  where f (Coord y x) = Pos (fromIntegral y) (fromIntegral x)--Stub

filterThresAndDist :: Double -> Double -> MatT U8 C1 Gray -> [Coord] -> [Coord]
filterThresAndDist int dist mat ps = filter (\p -> f int mat p && g ps dist p) ps
  where
    f :: Double -> MatT U8 C1 Gray -> Coord -> Bool
    f int mat (Coord y x) = (fromIntegral $ pixelAt y x mat) >= int
    g :: [Coord] -> Double -> Coord -> Bool
    g ps dist p = all (\x -> x == 0 || x>d2) $ map (distSq p) ps -- not very clean. Comparison with all non-self is the best way.
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

main = maintrue

maintrue :: IO ()
maintrue = do
  img <- readImg "cell.jpg"
  let ps = findParticles (convert img)
  print (length ps)

demos :: IO ()
demos = do
  demo8

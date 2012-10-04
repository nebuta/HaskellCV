module Main where

import CV.Core
import CV.Filter
import CV.Demo

import Data.List (sort)

findParticles :: GrayImage -> [Pos]
findParticles = detect . prefilter
  where prefilter = gauss 3

detect :: GrayImage -> [Pos]
detect img = refinePos $ filterThresAndDist minInt minDist img $ findIndexMat cmpEqual img dilated
  where
    percentile = 10.0   -- in %
    minInt = fromIntegral $ intensities !! (floor $ percentile / 100.0 * fromIntegral (length intensities))
    minDist = 4
    dilated = dilate (fromStrEl (Ellipse 3 3)) img
    intensities = (reverse . sort . concat . pixels) img

findIndexMat :: CmpFun a b c -> MatT a b c -> MatT a b c-> [Coord]
findIndexMat f a b = findNonZero $ CV.Core.compare f a b

refinePos :: [Coord] -> [Pos]
refinePos cs = map f cs
  where f (Coord y x) = Pos (fromIntegral y) (fromIntegral x)--Stub

filterThresAndDist :: Double -> Double -> MatT U8 C1 Gray -> [Coord] -> [Coord]
filterThresAndDist int dist mat ps = filter (f int dist mat ps) ps
  where
    f :: Double -> Double -> MatT U8 C1 Gray -> [Coord] -> Coord -> Bool
    f int dist mat ps (Coord y x) = (fromIntegral $ pixelAt y x mat) >= int  -- Stub: add dist filter

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

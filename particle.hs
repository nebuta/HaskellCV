module Main where

import CV.Core
import CV.Filter
import CV.Demo

findParticles :: GrayImage -> [Pos]
findParticles = detect . prefilter
  where prefilter = gauss 3

detect :: GrayImage -> [Pos]
detect img = filterThresAndDist percentileThres distThres $ refinePos $ findIndexMat cmpEqual img dilated
  where
    percentileThres = 10    -- in %
    distThres = 4
    dilated = dilate (fromStrEl (Ellipse 3 3)) img

findIndexMat :: CmpFun a b c -> MatT a b c -> MatT a b c-> [Coord]
findIndexMat f a b = findNonZero $ CV.Core.compare f a b

refinePos :: [Coord] -> [Pos]
refinePos cs = map f cs
  where f (Coord y x) = Pos (fromIntegral y) (fromIntegral x)--Stub

filterThresAndDist :: Double -> Double -> [Pos] -> [Pos]
filterThresAndDist thres dist ps = filter (f thres dist ps) ps
  where
    f :: Double -> Double -> [Pos] -> Pos -> Bool
    f th di ps p = True -- Stub

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
  img <- readImg "test.jpg"
  let ps = findParticles (convert img)
  print (length ps)

demos :: IO ()
demos = do
  demo6

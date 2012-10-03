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

findIndexMat :: CmpFun a -> MatT a -> MatT a -> [Coord]
findIndexMat _ _ _ = []   -- Stub!!

refinePos :: [Coord] -> [Pos]
refinePos _ = []    --Stub

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

main = demos

maintrue :: IO ()
maintrue = do
  img <- readImg "cell.jpg"
  let ps = findParticles (toGray img)
  return ()

demos :: IO ()
demos = do
  demo5

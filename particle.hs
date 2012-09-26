module Main where

import CV.Core
import CV.Filter

getAt :: Pos -> Mat2D -> Pixel
getAt pos mat = GrayPixel 0 -- Stub

findParticles :: GrayImage -> [Pos]
findParticles = detect . prefilter
  where prefilter = apply (gauss 3)

detect :: GrayImage -> [Pos]
detect img = filterThresAndDist percentileThres distThres $ findIndexMat Equal img dilated
  where
    percentileThres = 10    -- in %
    distThres = 4
    dilated = dilate (Disc 3) img

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

main = do
  let imgs = map (monoColor (Channel B16) 300 300) [yellow,blue,red]
--  mapM_ readImg (replicate 100 "test.jpg")
  img <- readImg "test.jpg"
  let gray = fromImg img :: GrayImage     -- This is so cool! Automatic image conversion by polymorphic type.
  mapM_ showMat imgs
  showImg (apply (gauss 3) gray)

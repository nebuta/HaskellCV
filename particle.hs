module Main where

import CV.Core

getAt :: Pos -> Mat2D -> Pixel
getAt pos mat = GrayPixel 0 -- Stub

findParticles :: GrayImage -> [Pos]
findParticles = detect . prefilter

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

prefilter :: Iso GrayImage
prefilter = gauss 3

gauss :: (Image a) => Int -> Iso a
gauss sigma img = img -- stub

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
--  img <- readImg "test.jpg"
  mapM_ showMat imgs

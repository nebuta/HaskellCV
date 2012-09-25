module Main where

import CV.Core hiding (gauss)

data Pos3D = Pos3D {
  frame :: Int,
  xx :: Double,
  yy :: Double
}

data Pos = Pos {
  x :: Double,
  y :: Double
}


type Iso a = a -> a

data Color = RGB | Gray | HSV

type Ptr = Int -- stub

data Mat1D = Mat1D Ptr Color Int
data Mat2D = Mat2D Ptr Color Int Int -- width, height
data Mat3D = Mat3D Ptr Color Int Int Int

getAt :: Pos -> Mat2D -> Pixel
getAt pos mat = GrayPixel 0 -- Stub

data GrayImage = GrayImage Mat2D

data Pixel = RGBPixel Int Int Int | GrayPixel Int | HSVPixel Int Int Int deriving (Eq, Ord)

data Value = ValueD1 Double | ValueD3 Double Double Double | ValueI1 Int | ValueI3 Int Int Int

class Image a where
  mat :: a -> Mat2D
  pixel :: Pos -> a -> Pixel
  width :: a -> Int
  height :: a -> Int
  dim :: a -> (Int,Int)
  dim img = (width img,height img)

instance Image GrayImage where
  mat (GrayImage m) = m
--  pixel :: Pos -> GrayImage -> GrayPixel
  pixel pos (GrayImage m) = getAt pos m --stub

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

type Angle = Double

data Strel = Disc Double | Rect Double Double | Line Angle Double 

dilate :: (Image a) => Strel -> Iso a
dilate se img = img -- stub

data CompareFun = Equal | Smaller | Bigger | AnyFun (Pixel->Pixel->Bool)

findIndexMat :: (Image a) => CompareFun -> a -> a -> [Pos]
findIndexMat Equal imgA imgB = findIndexMat (AnyFun (==)) imgA imgB   --ToDo: replace with the built-in C++ function
findIndexMat Smaller imgA imgB = findIndexMat (AnyFun (<)) imgA imgB
findIndexMat Bigger imgA imgB = findIndexMat (AnyFun (>)) imgA imgB
findIndexMat (AnyFun predicate) imgA imgB
    | dim imgA == dim imgB = filter (f predicate) ps
    | otherwise = []
  where
    ps :: [Pos]
    ps = [Pos x y | x<-map fromIntegral [0..width imgA-1],y<-map fromIntegral [0..height imgA-1]]
    f :: (Pixel -> Pixel -> Bool) -> Pos -> Bool
    f pred pos = pred (pixel pos imgA) (pixel pos imgB)

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


main = putStrLn "stub"

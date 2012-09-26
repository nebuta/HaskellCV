-- Core.hs

{-# LANGUAGE ForeignFunctionInterface #-}

module CV.Core where

import Foreign.C -- get the C types
import Foreign.ForeignPtr (ForeignPtr,newForeignPtr)
import Foreign.ForeignPtr.Safe (withForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree)
import System.IO.Unsafe (unsafePerformIO)
import CV.FFI
import Data.ByteString (unpack)
import Data.String

type Iso a = a -> a

data Color = RGBColor | Gray | HSV

type Ptr = Int -- stub

data Mat1D = Mat1D Ptr Color Int
data Mat2D = Mat2D Ptr Color Int Int -- width, height
data Mat3D = Mat3D Ptr Color Int Int Int

data Filter a b = Filter (a -> b)
type IsoFilter a = Filter a a

data Pos3D = Pos3D {
  frame :: Int,
  xx :: Double,
  yy :: Double
}

data Pos = Pos {
  x :: Double,
  y :: Double
} deriving Show

instance Eq Mat where
  (Mat a) == (Mat b) = unsafePerformIO $ do
    withForeignPtr a $ \aa -> do
      withForeignPtr b $ \bb -> do
        if aa == bb then
          return True
        else do
          is_eq <- c_eqMat aa bb
          return (is_eq /= ci 0)

data GrayImage = GrayImage Mat

data Pixel = RGBPixel Int Int Int | GrayPixel Int | HSVPixel Int Int Int deriving (Eq, Ord)

data Value = ValueD1 Double | ValueD3 Double Double Double | ValueI1 Int | ValueI3 Int Int Int

type Angle = Double

data Strel = Disc Double | Rect Double Double | Line Angle Double 

dilate :: (Image a) => Strel -> Iso a
dilate se img = img -- stub

data GImage = GImage Mat  -- "Generic Image"

class Image a where
  mat :: a -> Mat
  fromMat :: Mat -> a
  pixel :: Pos -> a -> Pixel
  width :: a -> Int
  height :: a -> Int
  dim :: a -> (Int,Int)
  dim img = (width img,height img)
  fromImg :: (Image b) => b -> a 
  fromImg img = fromMat (mat img) -- default. Convert mat data if necessary.

instance Image GImage where
  mat (GImage m) = m
  fromMat m = GImage m

instance Image GrayImage where
  mat (GrayImage m) = m
  fromMat m = GrayImage m
  fromImg img = GrayImage (cvtColor rgbToGray (mat img))
--  pixel :: Pos -> GrayImage -> GrayPixel
--  pixel pos (GrayImage m) = getAt pos m --stub

data Bits = B32 | B16 | B8
data Channel = Channel Bits

data RGB = RGB Int Int Int

yellow, red, blue, green :: RGB
yellow = RGB 255 255 0
red = RGB 255 0 0
blue = RGB 0 0 255
green = RGB 0 255 0

ci :: Int -> CInt
ci = fromIntegral


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

-- Matrix operations

-- |Element-wise absolute value
cvAbs :: Mat -> Mat
cvAbs (Mat m) =
  unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      mat_ptr <- c_abs mm
      mat <- newForeignPtr finalizerFree mat_ptr
      return (Mat mat)

-- |Matrix addition
(+:+) :: Mat -> Mat -> Mat
(Mat a) +:+ (Mat b) 
  = unsafePerformIO $ do
      withForeignPtr a $ \aa -> do
        withForeignPtr b $ \bb -> do
          mat_ptr <- c_addMat aa bb
          mat <- newForeignPtr finalizerFree mat_ptr
          return (Mat mat)
          
-- |Matrix subtraction
(-:-) :: Mat -> Mat -> Mat
(Mat a) -:- (Mat b) 
  = unsafePerformIO $ do
      withForeignPtr a $ \aa -> do
        withForeignPtr b $ \bb -> do
          mat_ptr <- c_subMat aa bb
          mat <- newForeignPtr finalizerFree mat_ptr
          return (Mat mat)



-- blend :: Mat -> Mat -> Mat
-- blend (Mat a) (Mat b) = Mat $ fromIntegral $ c_addMat (fromIntegral a) (fromIntegral b)

monoColor :: Channel -> Int -> Int -> RGB -> Mat
monoColor c h w (RGB r g b)
  = unsafePerformIO $ do
      mat_ptr <- c_monoColor (ci w) (ci h) (ci b) (ci g) (ci r)
      mat <- newForeignPtr finalizerFree mat_ptr
      return (Mat mat)

showMat :: Mat -> IO ()
showMat (Mat m) = do
  withForeignPtr m $ \mm -> do
    c_showMat mm

showImg :: (Image a) => a -> IO ()
showImg = showMat . mat

{-
matsize :: Mat -> Int
matsize (Mat id) = fromIntegral (c_length (fromIntegral id))


-- Random Matrix with a specified size
randMat :: Int -> Int -> Mat
randMat y x = fromId (c_randMat (fromIntegral y) (fromIntegral x))

-- 1D vector with 0 norm
zeros :: Int -> Mat
zeros n = fromId (c_zeros (fromIntegral n))

{-
vals :: Mat -> [Int]
vals (Mat id) = map (fromIntegral . c_valAt (fromIntegral id)) (map fromIntegral [0..len-1])
  where
    len :: Int
    len = fromIntegral (c_length (fromIntegral id))
-}

-- 2D Filters
--



cd :: Double -> CDouble
cd = realToFrac


-- Color conversion
newtype ConvertCode = ConvertCode {unConvertCode :: CInt}

rgbToGray = ConvertCode (ci 7)

addWeighted :: Mat -> Double -> Mat -> Double -> Double -> Mat
addWeighted (Mat ma) alpha (Mat mb) beta gamma = fromId $ c_addWeighted (ci ma) (cd alpha) (ci mb) (cd beta) (cd gamma)


-}


-- Image operations

readImg :: FilePath -> IO GImage
readImg file = do
  withCString file $ \cstr_path -> do
    mat_ptr <- c_readImg cstr_path
    mat <- newForeignPtr finalizerFree mat_ptr 
    return $ GImage (Mat mat)

-- Image color conversion
--

data ConvertCode = ConvertCode CInt
rgbToGray = ConvertCode (ci 7)


cvtColor :: ConvertCode -> Mat -> Mat
cvtColor (ConvertCode code) (Mat m)
  = unsafePerformIO $ do
      withForeignPtr m $ \mm -> do
        mat_ptr <- c_cvtColor code mm
        mat <- newForeignPtr finalizerFree mat_ptr
        return (Mat mat)


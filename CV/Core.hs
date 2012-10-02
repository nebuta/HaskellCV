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

data Mat = Mat !(ForeignPtr CMat)

type Iso a = a -> a

data Color = RGBColor | Gray | HSV

{-
data Mat1D = Mat1D Ptr Color Int
data Mat2D = Mat2D Ptr Color Int Int -- width, height
data Mat3D = Mat3D Ptr Color Int Int Int
-}



apply :: Image a => Iso Mat -> a -> a
apply f img = fromMat $ f (mat img)



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

data Coord = Coord {
  xi :: Int,
  yi :: Int
}

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

cd :: Double -> CDouble
cd = realToFrac


data CmpFun = CmpFun CInt | MyCmpFun (Pixel->Pixel->Bool)

cmpEqual = CmpFun 0
cmpGT = CmpFun 1
cmpGE = CmpFun 2
cmpLT = CmpFun 3
cmpLE = CmpFun 4
cmpNE = CmpFun 5

compare :: CmpFun -> Mat -> Mat -> Mat
compare (CmpFun code) (Mat ma) (Mat mb)
  = unsafePerformIO $ do
      withForeignPtr ma $ \mma -> do
        withForeignPtr mb $ \mmb -> do
          mat_ptr <- c_compare mma mmb code
          mat <- newForeignPtr cmatFree mat_ptr
          return (Mat mat)

-- Matrix operations

-- |Element-wise absolute value
cvAbs :: Mat -> Mat
cvAbs (Mat m) =
  unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      mat_ptr <- c_abs mm
      mat <- newForeignPtr cmatFree mat_ptr
      return (Mat mat)

-- |Matrix addition
(+:+) :: Mat -> Mat -> Mat
(Mat a) +:+ (Mat b) 
  = unsafePerformIO $ do
      withForeignPtr a $ \aa -> do
        withForeignPtr b $ \bb -> do
          mat_ptr <- c_addMat aa bb
          mat <- newForeignPtr cmatFree mat_ptr
          return (Mat mat)
          
-- |Matrix subtraction
(-:-) :: Mat -> Mat -> Mat
(Mat a) -:- (Mat b) 
  = unsafePerformIO $ do
      withForeignPtr a $ \aa -> do
        withForeignPtr b $ \bb -> do
          mat_ptr <- c_subMat aa bb
          mat <- newForeignPtr cmatFree mat_ptr
          return (Mat mat)

-- |Matrix element-wise multiplication
(*:*) :: Mat -> Mat -> Mat
(Mat a) *:* (Mat b) 
  = unsafePerformIO $ do
      withForeignPtr a $ \aa -> do
        withForeignPtr b $ \bb -> do
          mat_ptr <- c_mulMat aa bb
          mat <- newForeignPtr cmatFree mat_ptr
          return (Mat mat)

-- |Matrix division by a scalar
(/:) :: (Real a) => Mat -> a -> Mat
(Mat a) /: denom
  = unsafePerformIO $ do
      withForeignPtr a $ \aa -> do
        mat_ptr <- c_divNum aa (CDouble (realToFrac denom))
        mat <- newForeignPtr cmatFree mat_ptr
        return (Mat mat)

-- |Matrix element-wise division
(/:/) :: Mat -> Mat -> Mat
(Mat a) /:/ (Mat b) 
  = unsafePerformIO $ do
      withForeignPtr a $ \aa -> do
        withForeignPtr b $ \bb -> do
          mat_ptr <- c_divMat aa bb
          mat <- newForeignPtr cmatFree mat_ptr
          return (Mat mat)


-- blend :: Mat -> Mat -> Mat
-- blend (Mat a) (Mat b) = Mat $ fromIntegral $ c_addMat (fromIntegral a) (fromIntegral b)

monoColor :: Channel -> Int -> Int -> RGB -> Mat
monoColor c h w (RGB r g b)
  = unsafePerformIO $ do
      mat_ptr <- c_monoColor (ci w) (ci h) (ci b) (ci g) (ci r)
      mat <- newForeignPtr cmatFree mat_ptr
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
    mat <- newForeignPtr cmatFree mat_ptr 
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
        mat <- newForeignPtr cmatFree mat_ptr
        return (Mat mat)


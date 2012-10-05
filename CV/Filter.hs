module CV.Filter where

import CV.Core
import CV.Types
import CV.FFI

import Foreign.C
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr,newForeignPtr)
import Foreign.ForeignPtr.Safe (withForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree)
import System.IO.Unsafe (unsafePerformIO)

data StrEl = Rect Int Int | Ellipse Int Int | Cross Int Int
fromStrEl :: StrEl -> MatT U8 C1
fromStrEl el =
  unsafePerformIO $ do
    mat_ptr <- c_getStructuringElement (t el) (ww el) (hh el)
    mat <- newForeignPtr cmatFree mat_ptr
    return (MatT mat)
  where     -- ToDo: too long and ugly, think about redesigning
    t (Rect _ _) = strelRect
    t (Ellipse _ _) = strelEllipse
    t (Cross _ _) = strelCross
    ww (Rect w _) = ci w
    ww (Ellipse w _) = ci w
    ww (Cross w _) = ci w
    hh (Rect _ h) = ci h
    hh (Cross _ h) = ci h
    hh (Ellipse _ h) = ci h

strelRect, strelCross, strelEllipse :: CInt
strelRect = 0
strelCross = 1
strelEllipse = 2


--
-- Functions that apply to Mat

-- ToDo: Use Maybe for results? (in case of exception raised)

-- Default for gaussian
gauss :: Double -> Iso (MatT a b)
gauss sigma = gaussian 0 0 sigma 0

-- Full feature gaussian
-- gaussian :: Int -> Int -> Double -> Double -> Iso Mat
-- gaussian kw kh sx sy = generalCFilter (c_gaussian (ci kw) (ci kh) (cd sx) (cd sy))

gaussian :: Int -> Int -> Double -> Double -> Iso (MatT a b)
gaussian kw kh sx sy = generalCFilter (c_gaussian (ci kw) (ci kh) (cd sx) (cd sy))

boxFilter :: Int -> Int -> Iso (MatT a b)
boxFilter kx ky = generalCFilter (c_boxFilter (ci kx) (ci ky))

-- ToDo: still has a bug? Check params before passing to OpenCV
derivFilter :: Int -> Int -> Int -> Iso (MatT a b)
derivFilter dx dy ksize = generalCFilter (c_derivFilter (ci dx) (ci dy) (ci ksize))

medianFilter :: Int -> Iso (MatT a b)
medianFilter ksize = generalCFilter (c_medianFilter (ci ksize))

laplacian :: Int -> Double -> Double -> Iso (MatT a b)
laplacian ksize scale delta = generalCFilter (c_laplacian (ci ksize) (cd scale) (cd delta))

bilateral :: Int -> Double -> Double -> Iso (MatT a b) 
bilateral d sigmaColor sigmaSpace = generalCFilter (c_bilateral (ci d) (cd sigmaColor) (cd sigmaSpace))

sobel :: Int -> Int -> Int -> Double -> Double -> Iso (MatT a b)
sobel dx dy ksize scale delta = generalCFilter (c_sobel (ci dx) (ci dy) (ci ksize) (cd scale) (cd delta))

generalCFilter :: (Ptr CMat -> IO (Ptr CMat)) -> Iso (MatT a b)
generalCFilter f (MatT m) =
  unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      mat_ptr <- f mm
      mat <- newForeignPtr cmatFree mat_ptr
      return (MatT mat)

-- Filters using another Mat

class Dilate a
instance Dilate U8
instance Dilate U16
instance Dilate S16
instance Dilate F32
instance Dilate F64

dilate :: (Dilate a) => MatT a b -> Iso (MatT a b)
dilate kernel = generalCFilter2 c_dilate kernel

erode :: (Dilate a) => MatT a b-> Iso (MatT a b)
erode kernel mat = generalCFilter2 c_dilate kernel mat

generalCFilter2 :: (Ptr CMat -> Ptr CMat -> IO (Ptr CMat)) -> MatT a b -> Iso (MatT c d)
generalCFilter2 f (MatT p) (MatT m) =
  unsafePerformIO $ do
    withForeignPtr p $ \pp -> do
      withForeignPtr m $ \mm -> do
        mat_ptr <- f pp mm
        mat <- newForeignPtr cmatFree mat_ptr
        return (MatT mat)

invariant :: Iso (MatT a b) -> MatT a b -> MatT U8 C1
invariant f mat = CV.Core.compare cmpEqual (f mat) mat


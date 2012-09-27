module CV.Filter where

import CV.Core
import CV.FFI

import Foreign.C
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr,newForeignPtr)
import Foreign.ForeignPtr.Safe (withForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree)
import System.IO.Unsafe (unsafePerformIO)

data StrEl = Rect Int Int | Ellipse Int Int | Cross Int Int
fromStrEl :: StrEl -> Mat
fromStrEl el =
  unsafePerformIO $ do
    mat_ptr <- c_getStructuringElement (t el) (ww el) (hh el)
    mat <- newForeignPtr cmatFree mat_ptr
    return (Mat mat)
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
gauss :: Double -> Iso Mat
gauss sigma = gaussian 0 0 sigma 0

-- Full feature gaussian
-- gaussian :: Int -> Int -> Double -> Double -> Iso Mat
-- gaussian kw kh sx sy = generalCFilter (c_gaussian (ci kw) (ci kh) (cd sx) (cd sy))

gaussian :: Int -> Int -> Double -> Double -> Iso Mat
gaussian kw kh sx sy (Mat m) =
  unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      mat_ptr <- (c_gaussian (ci kw) (ci kh) (cd sx) (cd sy)) mm
      mat <- newForeignPtr cmatFree mat_ptr
      return (Mat mat)

boxFilter :: Int -> Int -> Iso Mat
boxFilter kx ky = generalCFilter (c_boxFilter (ci kx) (ci ky))


-- ToDo: still has a bug? Check params before passing to OpenCV
derivFilter :: Int -> Int -> Int -> Iso Mat
derivFilter dx dy ksize = generalCFilter (c_derivFilter (ci dx) (ci dy) (ci ksize))

medianFilter :: Int -> Iso Mat
medianFilter ksize = generalCFilter (c_medianFilter (ci ksize))

laplacian :: Int -> Double -> Double -> Iso Mat
laplacian ksize scale delta = generalCFilter (c_laplacian (ci ksize) (cd scale) (cd delta))

bilateral :: Int -> Double -> Double -> Iso Mat
bilateral d sigmaColor sigmaSpace = generalCFilter (c_bilateral (ci d) (cd sigmaColor) (cd sigmaSpace))

sobel :: Int -> Int -> Int -> Double -> Double -> Iso Mat
sobel dx dy ksize scale delta = generalCFilter (c_sobel (ci dx) (ci dy) (ci ksize) (cd scale) (cd delta))

generalCFilter :: (Ptr CMat -> IO (Ptr CMat)) -> Iso Mat
generalCFilter f (Mat m) =
  unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      mat_ptr <- f mm
      mat <- newForeignPtr cmatFree mat_ptr
      return (Mat mat)

-- Filters using another Mat

dilate :: Mat -> Iso Mat
dilate kernel mat = generalCFilter2 c_dilate kernel mat

generalCFilter2 :: (Ptr CMat -> Ptr CMat -> IO (Ptr CMat)) -> Mat -> Iso Mat
generalCFilter2 f (Mat p) (Mat m) =
  unsafePerformIO $ do
    withForeignPtr p $ \pp -> do
      withForeignPtr m $ \mm -> do
        mat_ptr <- f pp mm
        mat <- newForeignPtr cmatFree mat_ptr
        return (Mat mat)
  
-- erode



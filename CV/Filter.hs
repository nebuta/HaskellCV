module CV.Filter where

import CV.Core
import CV.FFI

import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr,newForeignPtr)
import Foreign.ForeignPtr.Safe (withForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree)
import System.IO.Unsafe (unsafePerformIO)

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

-- dilate 
-- erode

generalCFilter :: (Ptr CMat -> IO (Ptr CMat)) -> Iso Mat
generalCFilter f (Mat m) =
  unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      mat_ptr <- f mm
      mat <- newForeignPtr cmatFree mat_ptr
      return (Mat mat)


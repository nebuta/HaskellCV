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

-- Default for gaussian
gauss :: Double -> Iso Mat
gauss sigma mat = gaussian 0 0 sigma 0 mat

-- Full feature gaussian
gaussian :: Int -> Int -> Double -> Double -> Iso Mat
gaussian kw kh sx sy = generalCFilter (c_gaussian (ci kw) (ci kh) (cd sx) (cd sy))

boxFilter :: Int -> Int -> Iso Mat
boxFilter kx ky = generalCFilter (c_boxFilter (ci kx) (ci ky))

derivFilter :: Int -> Int -> Int -> Iso Mat
derivFilter dx dy ksize = generalCFilter (c_derivFilter (ci dx) (ci dy) (ci ksize))

medianFilter :: Int -> Iso Mat
medianFilter ksize = generalCFilter (c_medianFilter (ci ksize))

-- dilate 
-- erode

generalCFilter :: (Ptr CMat -> IO (Ptr CMat)) -> Iso Mat
generalCFilter f (Mat m) =
  unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      mat_ptr <- f mm
      mat <- newForeignPtr cmatFree mat_ptr
      return (Mat mat)

{-

-- BFilter :: built-in filter.
data BFilter = Gauss Int Int Double Double | BoxFilter Int Int | DerivFilter Int Int Int | Median Int

applyB :: BFilter -> Iso Mat
applyB (Gauss kx ky s1 s2) = gaussian kx ky s1 s2
applyB (BoxFilter kx ky) = boxFilter kx ky
applyB (DerivFilter dx dy ksize) = derivFilter dx dy ksize
applyB (Median 


-}


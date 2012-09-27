module CV.Filter where

import CV.Core
import CV.FFI

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
gaussian kw kh sx sy (Mat m) =
  unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      mat_ptr <- c_gaussian (ci kw) (ci kh) (cd sx) (cd sy) mm
      mat <- newForeignPtr cmatFree mat_ptr
      return (Mat mat)


boxFilter :: Int -> Int -> Iso Mat
boxFilter kx ky (Mat m) = 
  unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      mat_ptr <- c_boxFilter (ci kx) (ci ky) mm
      mat <- newForeignPtr cmatFree mat_ptr
      return (Mat mat)
      

-- BFilter :: built-in filter.
data BFilter = Gauss Int Int Double Double | BoxFilter Int Int

applyB :: BFilter -> Mat -> Mat
applyB (Gauss kx ky s1 s2) mat = gaussian kx ky s1 s2 mat
applyB (BoxFilter kx ky) mat = boxFilter kx ky mat


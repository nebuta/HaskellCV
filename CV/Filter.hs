module CV.Filter where

import CV.Core
import CV.FFI

import Foreign.ForeignPtr (ForeignPtr,newForeignPtr)
import Foreign.ForeignPtr.Safe (withForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree)
import System.IO.Unsafe (unsafePerformIO)


apply :: Image a => Iso Mat -> a -> a
apply f img = fromMat $ f (mat img)

--
-- Functions that apply to Mat

-- Default for gaussian
gauss :: Int -> Iso Mat
gauss sigma mat = gaussian 0 0 sigma 0 mat

-- Full feature gaussian
gaussian :: Int -> Int -> Int -> Int -> Iso Mat
gaussian kw kh sx sy (Mat m) =
  unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      mat_ptr <- c_gaussian (ci kw) (ci kh) (ci sx) (ci sy) mm
      mat <- newForeignPtr finalizerFree mat_ptr
      return (Mat mat)



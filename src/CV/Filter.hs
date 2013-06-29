{-# LANGUAGE TypeSynonymInstances #-}

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

imadjust :: Double -> Double -> Double -> Double -> IsoFilter U8 C1
imadjust li hi lo ho = generalCFilter (c_imadjust (cd li) (cd hi) (cd lo) (cd ho))

-- Default for gaussian
gauss :: Double -> IsoFilter a b
gauss sigma = gaussian 0 0 sigma 0

-- Full feature gaussian
-- gaussian :: Int -> Int -> Double -> Double -> Iso Mat
-- gaussian kw kh sx sy = generalCFilter (c_gaussian (ci kw) (ci kh) (cd sx) (cd sy))


gaussian :: Int -> Int -> Double -> Double -> IsoFilter a b
gaussian kw kh sx sy = generalCFilter (c_gaussian (ci kw) (ci kh) (cd sx) (cd sy))

-- This is very ad hoc. Need a more general way.
{-# RULES
"gaussian/gaussian" forall kw kh sx sy kw2 kh2 sx2 sy2 m. gaussian kw kh sx sy (gaussian kw2 kh2 sx2 sy2 m) = generalCFiltersInPlace [(c_gaussian (ci kw2) (ci kh2) (cd sx2) (cd sy2)) ,(c_gaussian (ci kw) (ci kh) (cd sx) (cd sy))] m
  #-}

boxFilter :: Int -> Int -> IsoFilter a b
boxFilter kx ky = generalCFilter (c_boxFilter (ci kx) (ci ky))

-- ToDo: still has a bug? Check params before passing to OpenCV
derivFilter :: Int -> Int -> Int -> IsoFilter a b
derivFilter dx dy ksize = generalCFilter (c_derivFilter (ci dx) (ci dy) (ci ksize))

medianFilter :: Int -> IsoFilter a b
medianFilter ksize = generalCFilter (c_medianFilter (ci ksize))

laplacian :: Int -> Double -> Double -> IsoFilter a b
laplacian ksize scale delta = generalCFilter (c_laplacian (ci ksize) (cd scale) (cd delta))

bilateral :: Int -> Double -> Double -> IsoFilter a b 
bilateral d sigmaColor sigmaSpace = generalCFilter (c_bilateral (ci d) (cd sigmaColor) (cd sigmaSpace))

sobel :: Int -> Int -> Int -> Double -> Double -> IsoFilter a b
sobel dx dy ksize scale delta = generalCFilter (c_sobel (ci dx) (ci dy) (ci ksize) (cd scale) (cd delta))


generalCFiltersInPlace :: [Ptr CMat -> Int -> IO (Ptr CMat)] -> IsoFilter a b
generalCFiltersInPlace fs (MatT m) =
  unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      mat_ptr <- (head fs) mm 0 -- 0 means "not in-place": Copy only for the first filter.
      mapM_ (\f -> f mat_ptr 1) (tail fs) -- 1 means "in-place"
      mat <- newForeignPtr cmatFree mat_ptr
      return (MatT m)

generalCFilter :: (Ptr CMat -> Int -> IO (Ptr CMat)) -> IsoFilter a b
generalCFilter f (MatT m) =
  unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      mat_ptr <- f mm 0 -- 0 means "not in-place"
      mat <- newForeignPtr cmatFree mat_ptr
      return (MatT mat)

-- Filters using another Mat

class Dilate a
instance Dilate U8
instance Dilate U16
instance Dilate S16
instance Dilate F32
instance Dilate F64

dilate :: (Dilate a) => MatT a b -> IsoFilter a b
dilate kernel = generalCFilter2 c_dilate kernel

erode :: (Dilate a) => MatT a b-> IsoFilter a b
erode kernel mat = generalCFilter2 c_dilate kernel mat

generalCFilter2 :: (Ptr CMat -> Ptr CMat -> IO (Ptr CMat)) -> MatT a b -> Iso (MatT c d)
generalCFilter2 f (MatT p) (MatT m) =
  unsafePerformIO $ do
    withForeignPtr p $ \pp -> do
      withForeignPtr m $ \mm -> do
        mat_ptr <- f pp mm
        mat <- newForeignPtr cmatFree mat_ptr
        return (MatT mat)

invariant :: IsoFilter a b -> MatT a b -> MatT U8 C1
invariant f mat = CV.Core.compare cmpEqual (f mat) mat


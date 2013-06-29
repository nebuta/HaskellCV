-- Core.hs

{-# LANGUAGE ForeignFunctionInterface, GADTs, MultiParamTypeClasses, FlexibleInstances #-}

module CV.Core where

import Foreign.C -- get the C types
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr,newForeignPtr)
import Foreign.ForeignPtr.Safe (withForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree)
import System.IO.Unsafe (unsafePerformIO)

import CV.Types
import CV.FFI
-- import CV.Instance

import Data.ByteString (unpack)
import Data.String
import qualified Data.Map as M (lookup,fromList)
import Data.Maybe (fromMaybe)

import Foreign.Marshal.Array (peekArray,advancePtr)
import Foreign.Marshal.Alloc (free)
import Foreign.C.Types
import Foreign.Storable (peekElemOff)

import Data.Word

-- * Internal
ci :: (Integral a)=> a -> CInt
ci = fromIntegral

cd :: Double -> CDouble
cd = realToFrac

cf :: Double -> CFloat
cf = realToFrac

type IsoFilter a b = MatT a b -> MatT a b

instance Eq (MatT a b) where
  (MatT a) == (MatT b) = unsafePerformIO $ do
    withForeignPtr a $ \aa -> do
      withForeignPtr b $ \bb -> do
        if aa == bb then
          return True
        else do
          is_eq <- c_eqMat aa bb
          return (is_eq /= ci 0)


-- * Predefined colors
yellow, red, blue, green :: RGBT U8
yellow = RGBT 255 255 0
red = RGBT 255 0 0
blue = RGBT 0 0 255
green = RGBT 0 255 0

-- * Color arithmetic

-- Stub: does not consider overflow
addColor :: (RGBTRange a) => RGBT a -> RGBT a -> RGBT a
addColor (RGBT r1 g1 b1) (RGBT r2 g2 b2) = RGBT (r1+r2) (g1+g2) (b1+b2)

subColor :: (RGBTRange a) => RGBT a -> RGBT a -> RGBT a
subColor (RGBT r1 g1 b1) (RGBT r2 g2 b2) = RGBT (r1-r2) (g1-g2) (b1-b2)

-- * Create matrix
--Create mat
--

monoColor :: Int -> Int -> RGBT U8 -> MatT U8 C3BGR
monoColor h w (RGBT r g b)
  = unsafePerformIO $ do
      mat_ptr <- c_monoColor (ci w) (ci h) (ci b) (ci g) (ci r)
      mat <- newForeignPtr cmatFree mat_ptr
      return (MatT mat)


-- * Get dimensions and other info
-- Matrix info
--

numRows :: MatT a b -> Int
numRows (MatT m) = unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      t <- c_rows mm
      return (fromIntegral t)

numCols :: MatT a b -> Int
numCols (MatT m) = unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      t <- c_cols mm
      return (fromIntegral t)


matType :: MatT a b -> CMatType
matType (MatT m) = 
  unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      t <- c_type mm
      return (CMatType t)

numChannels :: MatT a b -> Int
numChannels (MatT m) = unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      num <- c_channels mm
      return (fromIntegral num)

-- * Matrix arithmetic
-- |Element-wise absolute value
cvAbs :: MatT a b -> MatT a b
cvAbs (MatT m) =
  unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      mat_ptr <- c_abs mm
      mat <- newForeignPtr cmatFree mat_ptr
      return (MatT mat)

-- |Matrix addition
(+:+) :: MatT a b -> MatT a b -> MatT a b
(MatT a) +:+ (MatT b) 
  = unsafePerformIO $ do
      withForeignPtr a $ \aa -> do
        withForeignPtr b $ \bb -> do
          mat_ptr <- c_addMat aa bb
          mat <- newForeignPtr cmatFree mat_ptr
          return (MatT mat)
          
-- |Matrix subtraction
(-:-) :: MatT a b -> MatT a b -> MatT a b
(MatT a) -:- (MatT b) 
  = unsafePerformIO $ do
      withForeignPtr a $ \aa -> do
        withForeignPtr b $ \bb -> do
          mat_ptr <- c_subMat aa bb
          mat <- newForeignPtr cmatFree mat_ptr
          return (MatT mat)


-- |Matrix element-wise multiplication. Currently only the same type
(*:*) :: MatT a b -> MatT a b -> MatT a b
(MatT a) *:* (MatT b) = unsafePerformIO $ do
  withForeignPtr a $ \aa -> do
    withForeignPtr b $ \bb -> do
      mat_ptr <- c_mulMat aa bb
      mat <- newForeignPtr cmatFree mat_ptr
      return (MatT mat)

-- |Matrix division by a scalar
(/:) :: (Real c) => MatT a b -> c -> MatT a b
(MatT a) /: denom
  = unsafePerformIO $ do
      withForeignPtr a $ \aa -> do
        mat_ptr <- c_divNum aa (CDouble (realToFrac denom))
        mat <- newForeignPtr cmatFree mat_ptr
        return (MatT mat)

-- |Matrix element-wise division
-- ToDo: Is this correct? the same type for a return value??
(/:/) :: MatT a b -> MatT a b -> MatT a b
(MatT a) /:/ (MatT b) 
  = unsafePerformIO $ do
      withForeignPtr a $ \aa -> do
        withForeignPtr b $ \bb -> do
          mat_ptr <- c_divMat aa bb
          mat <- newForeignPtr cmatFree mat_ptr
          return (MatT mat)


-- |Crop matrix
crop :: MatT a b -> Int -> Int -> Int -> Int -> MatT a b
crop (MatT a) x y w h
  = unsafePerformIO $ do
      withForeignPtr a $ \aa -> do
        mat_ptr <- c_crop aa (ci x) (ci y) (ci w) (ci h)
        mat <- newForeignPtr cmatFree mat_ptr
        return (MatT mat)

-- blend :: Mat -> Mat -> Mat
-- blend (Mat a) (Mat b) = Mat $ fromIntegral $ c_addMat (fromIntegral a) (fromIntegral b)


showMatT :: MatT a b -> IO ()
showMatT (MatT m) = do
  withForeignPtr m $ \mm -> do
    c_showMat mm 0

showMatT' :: MatT a b -> Int -> IO ()
showMatT' (MatT m) delay = do
  withForeignPtr m $ \mm -> do
    c_showMat mm (ci delay)


-- addWeighted :: Mat -> Double -> Mat -> Double -> Double -> Mat
-- addWeighted (Mat ma) alpha (Mat mb) beta gamma = fromId $ c_addWeighted (ci ma) (cd alpha) (ci mb) (cd beta) (cd gamma)

-- Image operations



-- Histogram and statistics
--

-- * Histogram and statistics
--
type BinMin = Double
type BinMax = Double
type Frequency = Int
data Histogram = Histogram [(BinMin,BinMax,Frequency)] deriving Show


-- |Calculate histogram.
-- |Supports only C1 images for now.
histogram :: Int -> Double -> Double -> MatT a C1 -> Histogram
histogram numBin min max (MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    int_ptr <- c_hist 0 (ci numBin) (cf min) (cf max) mm
    vs <- peekArray numBin int_ptr
    let hist = Histogram (f numBin min max (map fromIntegral vs))
    return hist
      where
        f nb min mx vs = zip3 [min,(min+d)..] [(min+d),(min+d*2)..] vs
        d = (max-min)/(fromIntegral numBin)



equalizeHist :: IsoFilter U8 C1
equalizeHist (MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    mat_ptr <- c_equalizeHist mm
    mat <- newForeignPtr cmatFree mat_ptr
    return (MatT mat)


-- * Comparison and search

data CmpFun a b = CmpFun CInt | MyCmpFun (PixelType a b->PixelType a b->Bool)

cmpEqual = CmpFun 0
cmpGT = CmpFun 1
cmpGE = CmpFun 2
cmpLT = CmpFun 3
cmpLE = CmpFun 4
cmpNE = CmpFun 5

compare :: CmpFun a b -> MatT a b -> MatT a b -> MatT U8 C1 
compare (CmpFun code) (MatT ma) (MatT mb)
  = unsafePerformIO $ do
      withForeignPtr ma $ \mma -> do
        withForeignPtr mb $ \mmb -> do
          mat_ptr <- c_compare mma mmb code
          mat <- newForeignPtr cmatFree mat_ptr
          return (MatT mat)  

findNonZero :: (ChannelC1 b) => MatT a b -> [Coord]
findNonZero (MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    ptr <- c_findNonZero mm
    len <- fmap fromIntegral $ peekElemOff ptr 0    --ptr[0] holds the # of coord
    cy <- fmap (map fromIntegral) $ peekArray len (advancePtr ptr 1)
    cx <- fmap (map fromIntegral) $ peekArray len (advancePtr ptr (len+1))
    free ptr
    return (zipWith Coord cy cx)

regionalMax :: MatT a C1 -> GrayImage
regionalMax (MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    ptr <- c_regionalMax mm
    mat <- newForeignPtr cmatFree ptr
    return (MatT mat)


-- * Video I/O

newVideo :: FilePath -> VideoCodec -> Double -> Int -> Int -> IO VideoWriter
newVideo path codec fps height width = do
  withCString path $ \path_str -> do
    withCString (unCodec codec) $ \codec_str -> do
      vw_ptr <- c_newVideoWriter path_str codec_str (cd fps) (ci height) (ci width)
      vw <- newForeignPtr cvideoWriterFree vw_ptr
      return (VideoWriter vw)

addFrame :: VideoWriter -> MatT a b -> IO ()
addFrame (VideoWriter v) (MatT m) = do
  -- ToDo: check the dimension matching
  withForeignPtr m $ \mm -> do
    withForeignPtr v $ \vv -> do
      c_videoWrite vv mm

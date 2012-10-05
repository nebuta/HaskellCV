-- Core.hs

{-# LANGUAGE ForeignFunctionInterface, GADTs, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

module CV.Core where

import Foreign.C -- get the C types
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr,newForeignPtr)
import Foreign.ForeignPtr.Safe (withForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree)
import System.IO.Unsafe (unsafePerformIO)
import CV.FFI
import Data.ByteString (unpack)
import Data.String
import qualified Data.Map as M (lookup,fromList)
import Data.Maybe (fromMaybe)

import Foreign.Marshal.Array (peekArray,advancePtr)
import Foreign.Marshal.Alloc (free)
import Foreign.C.Types
import Foreign.Storable (peekElemOff)

data Mat = Mat !(ForeignPtr CMat)


-- For MatT phantom types
-- ToDo: AnyDepth, AnyChannel, AnyColor should only be used for a Mat returned from readImg 
data AnyDepth
data U8 
data S8 
data U16
data S16
data S32
data F32
data F64

class DepthType a
instance DepthType AnyDepth
instance DepthType U8
instance DepthType S8
instance DepthType U16
instance DepthType S16
instance DepthType S32
instance DepthType F32
instance DepthType F64

class (DepthType a) => DepthInt a
instance DepthInt U8
instance DepthInt S8
instance DepthInt U16
instance DepthInt S16
instance DepthInt S32

class (DepthType a) => DepthFloat a
instance DepthFloat F32
instance DepthFloat F64


data AnyChannel
data C1
data C1Gray
data C2
data C3
data C3BGR
data C3XYZ
data C3Luv
data C3YCrCb
data C3HSV
data C3HLS
data C3Lab
data C4
data C4BGRA
data CN

class ChannelType a
instance ChannelType AnyChannel
instance ChannelType C1
instance ChannelType C1Gray
instance ChannelType C2
instance ChannelType C3
instance ChannelType C3BGR
instance ChannelType C3XYZ
instance ChannelType C3Luv
instance ChannelType C3YCrCb
instance ChannelType C3HSV
instance ChannelType C3HLS
instance ChannelType C3Lab
instance ChannelType C4
instance ChannelType C4BGRA
instance ChannelType CN

class (ChannelType a) => ChannelC1 a
instance ChannelC1 C1

class (ChannelType a) => ChannelC3 a
instance ChannelC3 C3
instance ChannelC3 C3BGR
instance ChannelC3 C3XYZ
instance ChannelC3 C3Luv
instance ChannelC3 C3YCrCb
instance ChannelC3 C3HSV
instance ChannelC3 C3HLS
instance ChannelC3 C3Lab

newtype CDepth = CDepth {unDepth :: CInt}
d_u8 = CDepth 0
d_s8 = CDepth 1
d_u16 = CDepth 2
d_s16 = CDepth 3
d_s32 = CDepth 4
d_f32 = CDepth 5
d_f64 = CDepth 6


-- Use of Phantom type
data MatT a b = MatT !(ForeignPtr CMat) -- stub e.g. MatT U8 C1Gray, MatT AnyPixel AnyChannel

newtype CMatType = CMatType {unCMatType :: CInt} deriving (Eq,Ord)
cv8UC1 = CMatType 0

type GrayImage = MatT U8 C1

type Iso a = a -> a

data Pos3D = Pos3D {
  frame :: Int,
  yy :: Double,
  xx :: Double
}

data Pos = Pos {
  y :: Double,
  x :: Double
} deriving Show

data Coord = Coord {
  yi :: Int,
  xi :: Int
} deriving (Eq,Show)

class Positional a where
  dist :: a -> a -> Double

instance Positional Pos where
  dist (Pos y1 x1) (Pos y2 x2) = (y1-y2)*(y1-y2) + (x1-x2)*(x1-x2)

instance Positional Coord where
  dist (Coord y1 x1) (Coord y2 x2) = fromIntegral ( (y1-y2)*(y1-y2) + (x1-x2)*(x1-x2) )

instance Eq Mat where
  (Mat a) == (Mat b) = unsafePerformIO $ do
    withForeignPtr a $ \aa -> do
      withForeignPtr b $ \bb -> do
        if aa == bb then
          return True
        else do
          is_eq <- c_eqMat aa bb
          return (is_eq /= ci 0)

type Angle = Double

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

cf :: Double -> CFloat
cf = realToFrac

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

-- Histogram functions
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

-- Phantom types conversion  Not safe!!!
forceCast :: MatT a b -> MatT c d
forceCast (MatT m) = MatT m

-- blend :: Mat -> Mat -> Mat
-- blend (Mat a) (Mat b) = Mat $ fromIntegral $ c_addMat (fromIntegral a) (fromIntegral b)

monoColor :: Int -> Int -> RGB -> MatT U8 C3BGR
monoColor h w (RGB r g b)
  = unsafePerformIO $ do
      mat_ptr <- c_monoColor (ci w) (ci h) (ci b) (ci g) (ci r)
      mat <- newForeignPtr cmatFree mat_ptr
      return (MatT mat)

showMatT :: MatT a b -> IO ()
showMatT (MatT m) = do
  withForeignPtr m $ \mm -> do
    c_showMat mm

-- addWeighted :: Mat -> Double -> Mat -> Double -> Double -> Mat
-- addWeighted (Mat ma) alpha (Mat mb) beta gamma = fromId $ c_addWeighted (ci ma) (cd alpha) (ci mb) (cd beta) (cd gamma)

-- Image operations

readImg :: FilePath -> IO (MatT a b)
readImg file = do
  withCString file $ \cstr_path -> do
    mat_ptr <- c_readImg cstr_path
    mat <- newForeignPtr cmatFree mat_ptr 
    return $ MatT mat

-- Image color conversion
--

newtype ConvertCode = ConvertCode {unConvertCode :: CInt}
-- data ConvertCode = ConvertCode CInt
bgrToGray = ConvertCode (ci 6)
rgbToGray = ConvertCode (ci 7)

cvtDepth' :: CDepth -> MatT a b -> MatT c b
cvtDepth' depth (MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    mat_ptr <- m_changeDepth (unDepth depth) mm
    mat <- newForeignPtr cmatFree mat_ptr 
    return $ MatT mat

cvtColor' :: ConvertCode -> MatT a b -> MatT a c
cvtColor' (ConvertCode code) (MatT m)
  = unsafePerformIO $ do
      withForeignPtr m $ \mm -> do
        mat_ptr <- c_cvtColor code mm
        mat <- newForeignPtr cmatFree mat_ptr
        return (MatT mat)

-- conversion of depth
class CvtDepth from to where
  cvtDepth :: MatT from a -> MatT to a

-- conversion of channels, keep depth (a)
class CvtColor from to where
  cvtColor :: MatT a from -> MatT a to

-- Conversion between any compatible mat's. Two can have different depths and channels.
class (CvtDepth a c, CvtColor b d) => Convert a b c d where
  convert :: MatT a b -> MatT c d
  convert = cvtColor . cvtDepth

instance CvtDepth a U8 where
  cvtDepth = cvtDepth' d_u8

instance CvtDepth a U16 where
  cvtDepth mat = cvtDepth' d_u16 mat

instance CvtColor C3BGR C1 where
  cvtColor = cvtColor' bgrToGray

instance CvtColor a C1 where
  cvtColor mat@(MatT m) = case numChannels mat of
                3 -> cvtColor' bgrToGray mat   -- Assuming BGR. since AnyChannel is returned only from readImg.
                1 -> (MatT m) :: MatT a C1 
                _ -> error "Only C1 or C3 can be converted to C1"

instance Convert a b U8 C1  -- Just use a default implementation

class Pixel a b where
  type PixelType a b :: *
  pixelAt :: Int -> Int -> MatT a b -> PixelType a b
  pixels :: MatT a b -> [[PixelType a b]]
  findPixels :: PixelType a b -> MatT a b -> [Coord]
  percentile :: Double -> MatT a b -> PixelType a b

instance Pixel U8 C1 where
  type PixelType U8 C1 = Int
  pixelAt = pixelIntAt
  pixels = pixelsInt
  percentile = percentileInt

--ToDo: check if this is correct.
pixelsInt :: (ChannelC1 b) => MatT a b -> [[Int]]   --Single channel
pixelsInt mat@(MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    let nr = (numRows mat)
    let nc = (numCols mat)
    pp <- c_valsUChar mm
    row_ptrs <- peekArray nr pp
    val <- mapM (peekArray nc) row_ptrs
    free pp  --ToDo: Is this good?
    return (map (map fromIntegral) val)

pixelIntAt  :: Int -> Int -> MatT a b -> Int
pixelIntAt y x (MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    val <- c_pixelIntAt (ci y) (ci x) mm
    return (fromIntegral val)

percentileInt :: (DepthInt a) => Double -> MatT a b -> Int
percentileInt perc (MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    val <- c_percentileInt (cd perc) mm
    return (fromIntegral val)

percentileFloat :: (DepthFloat a) => Double -> MatT a b -> Double
percentileFloat perc (MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    val <- c_percentileFloat (cd perc) mm
    return (realToFrac val)

findNonZero :: (ChannelC1 b) => MatT a b -> [Coord]
findNonZero (MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    ptr <- c_findNonZero mm
    len <- fmap fromIntegral $ peekElemOff ptr 0    --ptr[0] holds the # of coord
    cy <- fmap (map fromIntegral) $ peekArray len (advancePtr ptr 1)
    cx <- fmap (map fromIntegral) $ peekArray len (advancePtr ptr (len+1))
    free ptr
    return (zipWith Coord cy cx)
    
  

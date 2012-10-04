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

data AnyChannel
data C1
data C2
data C3
data C4
data Ch = Ch Int

class ChannelType a
instance ChannelType C1
instance ChannelType C2
instance ChannelType C3
instance ChannelType C4
instance ChannelType Ch

data AnyColor
data RGBColor
data HSV
data Gray

class ColorType a
instance ColorType RGBColor
instance ColorType HSV
instance ColorType Gray

newtype CDepth = CDepth {unDepth :: CInt}
d_u8 = CDepth 0
d_s8 = CDepth 1
d_u16 = CDepth 2
d_s16 = CDepth 3
d_s32 = CDepth 4
d_f32 = CDepth 5
d_f64 = CDepth 6


-- Use of Phantom type
data MatT a b c = MatT !(ForeignPtr CMat) -- stub e.g. MatT U8 C1 Gray, MatT AnyPixel AnyChannel AnyColor
data Scalar = Scalar !(ForeignPtr CScalar)

newtype CMatType = CMatType {unCMatType :: CInt} deriving (Eq,Ord)
data MatType = CV_8UC1 | CV_8UC2 | CV_8UC3 | CV_16UC1 | AnyPixel deriving (Eq,Ord)

type GrayImage = MatT U8 C1 Gray

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

data CmpFun a b c = CmpFun CInt | MyCmpFun (PixelType a b c->PixelType a b c->Bool)

cmpEqual = CmpFun 0
cmpGT = CmpFun 1
cmpGE = CmpFun 2
cmpLT = CmpFun 3
cmpLE = CmpFun 4
cmpNE = CmpFun 5

compare :: CmpFun a b c -> MatT a b c -> MatT a b c -> MatT U8 C1 Gray
compare (CmpFun code) (MatT ma) (MatT mb)
  = unsafePerformIO $ do
      withForeignPtr ma $ \mma -> do
        withForeignPtr mb $ \mmb -> do
          mat_ptr <- c_compare mma mmb code
          mat <- newForeignPtr cmatFree mat_ptr
          return (MatT mat)  

-- Matrix info
--

numRows :: MatT a b c -> Int
numRows (MatT m) = unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      t <- c_rows mm
      return (fromIntegral t)

numCols :: MatT a b c -> Int
numCols (MatT m) = unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      t <- c_cols mm
      return (fromIntegral t)


matType :: MatT a b c -> MatType
matType (MatT m) = 
  unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      t <- c_type mm
      return (fromCMatType (CMatType t))

numChannels :: MatT a b c -> Int
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
histogram :: Int -> Double -> Double -> MatT a C1 c -> Histogram
histogram numBin min max (MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    int_ptr <- c_hist 0 (ci numBin) (cf min) (cf max) mm
    vs <- peekArray numBin int_ptr
    let hist = Histogram (f numBin min max (map fromIntegral vs))
    return hist
      where
        f nb min mx vs = zip3 [min,(min+d)..] [(min+d),(min+d*2)..] vs
        d = (max-min)/(fromIntegral numBin)

fromCMatType :: CMatType -> MatType
fromCMatType a = fromMaybe AnyPixel $ M.lookup a (M.fromList listMatType)

listMatType = [(CMatType 0,CV_8UC1)]

fromMatType :: MatType -> CMatType
fromMatType a = fromMaybe (CMatType (-1)) $ M.lookup a (M.fromList $ map f listMatType)
  where f (f,s) = (s,f)


-- |Element-wise absolute value
cvAbs :: MatT a b c -> MatT a b c
cvAbs (MatT m) =
  unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      mat_ptr <- c_abs mm
      mat <- newForeignPtr cmatFree mat_ptr
      return (MatT mat)

-- |Matrix addition
(+:+) :: MatT a b c -> MatT a b c -> MatT a b c
(MatT a) +:+ (MatT b) 
  = unsafePerformIO $ do
      withForeignPtr a $ \aa -> do
        withForeignPtr b $ \bb -> do
          mat_ptr <- c_addMat aa bb
          mat <- newForeignPtr cmatFree mat_ptr
          return (MatT mat)
          
-- |Matrix subtraction
(-:-) :: MatT a b c -> MatT a b c -> MatT a b c
(MatT a) -:- (MatT b) 
  = unsafePerformIO $ do
      withForeignPtr a $ \aa -> do
        withForeignPtr b $ \bb -> do
          mat_ptr <- c_subMat aa bb
          mat <- newForeignPtr cmatFree mat_ptr
          return (MatT mat)


-- |Matrix element-wise multiplication. Currently only the same type
(*:*) :: MatT a b c -> MatT a b c -> MatT a b c
(MatT a) *:* (MatT b) = unsafePerformIO $ do
  withForeignPtr a $ \aa -> do
    withForeignPtr b $ \bb -> do
      mat_ptr <- c_mulMat aa bb
      mat <- newForeignPtr cmatFree mat_ptr
      return (MatT mat)

-- |Matrix division by a scalar
(/:) :: (Real d) => MatT a b c -> d -> MatT a b c
(MatT a) /: denom
  = unsafePerformIO $ do
      withForeignPtr a $ \aa -> do
        mat_ptr <- c_divNum aa (CDouble (realToFrac denom))
        mat <- newForeignPtr cmatFree mat_ptr
        return (MatT mat)

-- |Matrix element-wise division
-- ToDo: Is this correct? the same type for a return value??
(/:/) :: MatT a b c -> MatT a b c -> MatT a b c
(MatT a) /:/ (MatT b) 
  = unsafePerformIO $ do
      withForeignPtr a $ \aa -> do
        withForeignPtr b $ \bb -> do
          mat_ptr <- c_divMat aa bb
          mat <- newForeignPtr cmatFree mat_ptr
          return (MatT mat)

-- Phantom types conversion  Not safe!!!
forceCast :: MatT a b c -> MatT d e f
forceCast (MatT m) = MatT m

-- blend :: Mat -> Mat -> Mat
-- blend (Mat a) (Mat b) = Mat $ fromIntegral $ c_addMat (fromIntegral a) (fromIntegral b)

monoColor :: Int -> Int -> RGB -> MatT U8 C3 RGBColor
monoColor h w (RGB r g b)
  = unsafePerformIO $ do
      mat_ptr <- c_monoColor (ci w) (ci h) (ci b) (ci g) (ci r)
      mat <- newForeignPtr cmatFree mat_ptr
      return (MatT mat)

showMatT :: MatT a b c -> IO ()
showMatT (MatT m) = do
  withForeignPtr m $ \mm -> do
    c_showMat mm

-- addWeighted :: Mat -> Double -> Mat -> Double -> Double -> Mat
-- addWeighted (Mat ma) alpha (Mat mb) beta gamma = fromId $ c_addWeighted (ci ma) (cd alpha) (ci mb) (cd beta) (cd gamma)

-- Image operations

readImg :: FilePath -> IO (MatT AnyDepth AnyChannel AnyColor)
readImg file = do
  withCString file $ \cstr_path -> do
    mat_ptr <- c_readImg cstr_path
    mat <- newForeignPtr cmatFree mat_ptr 
    return $ MatT mat

-- Image color conversion
--

newtype ConvertCode = ConvertCode {unConvertCode :: CInt}
-- data ConvertCode = ConvertCode CInt
rgbToGray = ConvertCode (ci 7)

cvtDepth' :: CDepth -> MatT a b c -> MatT d b c
cvtDepth' depth (MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    mat_ptr <- m_changeDepth (unDepth depth) mm
    mat <- newForeignPtr cmatFree mat_ptr 
    return $ MatT mat

cvtColor' :: ConvertCode -> MatT a b c -> MatT a d e
cvtColor' (ConvertCode code) (MatT m)
  = unsafePerformIO $ do
      withForeignPtr m $ \mm -> do
        mat_ptr <- c_cvtColor code mm
        mat <- newForeignPtr cmatFree mat_ptr
        return (MatT mat)

-- conversion of depth
class CvtDepth from to where
  cvtDepth :: MatT from a b -> MatT to a b

-- conversion of channels, keep depth (a)
class CvtColor from to where
  cvtColor :: MatT a from b -> MatT a to c

-- Conversion between any compatible mat's. Two can have different depths and channels.
class (CvtDepth a d, CvtColor b e) => Convert a b c d e f where
  convert :: MatT a b c -> MatT d e f
  convert = cvtColor . cvtDepth

class ConvertAny a b c where
  convertAny :: MatT AnyDepth AnyChannel AnyColor -> MatT a b c

instance CvtDepth a U8 where
  cvtDepth = cvtDepth' d_u8

instance CvtDepth a U16 where
  cvtDepth mat = cvtDepth' d_u16 mat

instance CvtColor AnyChannel C1 where
  cvtColor mat@(MatT m) = case numChannels mat of
                3 -> cvtColor' rgbToGray mat   -- Assuming RGB. since AnyChannel is returned only from readImg.
                1 -> (MatT m) :: MatT a C1 b  --ToDo :: This can be cast to any color, so this is wrong. Channel and color should be merged. Color info inherently contains channel number info.
                _ -> error "Only C1 or C3 can be converted to C1"

instance Convert AnyDepth AnyChannel AnyColor U8 C1 Gray

class Pixel a b c where
  type PixelType a b c :: *
  pixelAt :: Int -> Int -> MatT a b c -> PixelType a b c
  pixels :: MatT a b c -> [[PixelType a b c]]
  findPixels :: PixelType a b c -> MatT a b c -> [Coord]

instance Pixel U8 C1 Gray where
  type PixelType U8 C1 Gray = Int
  pixelAt = pixelIntAt
  pixels = pixelsInt

--ToDo: check if this is correct.
pixelsInt :: MatT a C1 c -> [[Int]]   --Single channel
pixelsInt mat@(MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    let nr = (numRows mat)
    let nc = (numCols mat)
    pp <- c_valsUChar mm
    row_ptrs <- peekArray nr pp
    val <- mapM (peekArray nc) row_ptrs
    free pp  --ToDo: Is this good?
    return (map (map fromIntegral) val)

pixelIntAt  :: Int -> Int -> MatT a b c -> Int
pixelIntAt y x (MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    val <- c_pixelIntAt (ci y) (ci x) mm
    return (fromIntegral val)

findNonZero :: MatT a C1 c -> [Coord]
findNonZero (MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    ptr <- c_findNonZero mm
    len <- fmap fromIntegral $ peekElemOff ptr 0    --ptr[0] holds the # of coord
    cy <- fmap (map fromIntegral) $ peekArray len (advancePtr ptr 1)
    cx <- fmap (map fromIntegral) $ peekArray len (advancePtr ptr (len+1))
    free ptr
    return (zipWith Coord cy cx)
    
  

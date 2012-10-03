-- Core.hs

{-# LANGUAGE ForeignFunctionInterface, GADTs, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

module CV.Core where

import Foreign.C -- get the C types
import Foreign.ForeignPtr (ForeignPtr,newForeignPtr)
import Foreign.ForeignPtr.Safe (withForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree)
import System.IO.Unsafe (unsafePerformIO)
import CV.FFI
import Data.ByteString (unpack)
import Data.String
import qualified Data.Map as M (lookup,fromList)
import Data.Maybe (fromMaybe)

data Mat = Mat !(ForeignPtr CMat)

data AnyPixel
data CV_8UC1
data CV_8UC2
data CV_8UC3
data CV_8UC4
data CV_16UC1

-- Use of Phantom type
data MatT a = MatT !(ForeignPtr CMat) -- stub
data PixelT a = RGBPixel Int Int Int | GrayPixel Int | HSVPixel Int Int Int deriving (Eq, Ord)
newtype CMatType = CMatType {unCMatType :: CInt} deriving (Eq,Ord)
data MatType = CV_8UC1 | CV_8UC2 | CV_8UC3 | AnyPixel deriving (Eq,Ord)

type GrayImage = MatT CV_8UC1

data Ch1 = Ch1
data Ch2
data Ch3
data Ch4

class Channel a
instance Channel Ch1

class Depth a
data D8
data D16
data D32
data D64
instance Depth D8
instance Depth D16
instance Depth D32
instance Depth D64


class ByteType a
data ByteU
data ByteS
data ByteF

type Iso a = a -> a

data Color = RGBColor | Gray | HSV

data Filter a b = Filter (a -> b)
type IsoFilter a = Filter a a

data Pos3D = Pos3D {
  frame :: Int,
  xx :: Double,
  yy :: Double
}

data Pos = Pos {
  x :: Double,
  y :: Double
} deriving Show

data Coord = Coord {
  xi :: Int,
  yi :: Int
}

instance Eq Mat where
  (Mat a) == (Mat b) = unsafePerformIO $ do
    withForeignPtr a $ \aa -> do
      withForeignPtr b $ \bb -> do
        if aa == bb then
          return True
        else do
          is_eq <- c_eqMat aa bb
          return (is_eq /= ci 0)

data Value = ValueD1 Double | ValueD3 Double Double Double | ValueI1 Int | ValueI3 Int Int Int

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

class MatArith a b where
  type MulType a b
  (*:*) :: MatT a -> MatT b -> MatT (MulType a b)
  -- |Matrix element-wise multiplication
  (MatT a) *:* (MatT b) 
     = unsafePerformIO $ do
      withForeignPtr a $ \aa -> do
        withForeignPtr b $ \bb -> do
          mat_ptr <- c_mulMat aa bb
          mat <- newForeignPtr cmatFree mat_ptr
          return (MatT mat)

instance MatArith CV_8UC1 AnyPixel where
  type MulType CV_8UC1 AnyPixel = AnyPixel

instance MatArith CV_8UC2 AnyPixel where
  type MulType CV_8UC2 AnyPixel = AnyPixel

instance MatArith CV_8UC3 AnyPixel where
  type MulType CV_8UC3 AnyPixel = AnyPixel

instance MatArith CV_8UC4 AnyPixel where
  type MulType CV_8UC4 AnyPixel = AnyPixel

instance MatArith CV_16UC1 AnyPixel where
  type MulType CV_16UC1 AnyPixel = AnyPixel

data CmpFun a = CmpFun CInt | MyCmpFun (PixelT a->PixelT a->Bool)

cmpEqual = CmpFun 0
cmpGT = CmpFun 1
cmpGE = CmpFun 2
cmpLT = CmpFun 3
cmpLE = CmpFun 4
cmpNE = CmpFun 5

compare :: CmpFun a -> MatT a -> MatT a -> MatT CV_8UC1
compare (CmpFun code) (MatT ma) (MatT mb)
  = unsafePerformIO $ do
      withForeignPtr ma $ \mma -> do
        withForeignPtr mb $ \mmb -> do
          mat_ptr <- c_compare mma mmb code
          mat <- newForeignPtr cmatFree mat_ptr
          return (MatT mat)

-- Matrix operations

matType :: MatT a -> MatType
matType (MatT m) = 
  unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      t <- c_type mm
      return (fromCMatType (CMatType t))

fromCMatType :: CMatType -> MatType
fromCMatType a = fromMaybe AnyPixel $ M.lookup a (M.fromList listMatType)

listMatType = [(CMatType 0,CV_8UC1)]

fromMatType :: MatType -> CMatType
fromMatType a = fromMaybe (CMatType (-1)) $ M.lookup a (M.fromList $ map f listMatType)
  where f (f,s) = (s,f)


-- |Element-wise absolute value
cvAbs :: MatT a -> MatT a
cvAbs (MatT m) =
  unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      mat_ptr <- c_abs mm
      mat <- newForeignPtr cmatFree mat_ptr
      return (MatT mat)

-- |Matrix addition
(+:+) :: MatT a -> MatT a -> MatT a
(MatT a) +:+ (MatT b) 
  = unsafePerformIO $ do
      withForeignPtr a $ \aa -> do
        withForeignPtr b $ \bb -> do
          mat_ptr <- c_addMat aa bb
          mat <- newForeignPtr cmatFree mat_ptr
          return (MatT mat)
          
-- |Matrix subtraction
(-:-) :: MatT a -> MatT a -> MatT a
(MatT a) -:- (MatT b) 
  = unsafePerformIO $ do
      withForeignPtr a $ \aa -> do
        withForeignPtr b $ \bb -> do
          mat_ptr <- c_subMat aa bb
          mat <- newForeignPtr cmatFree mat_ptr
          return (MatT mat)


-- |Matrix division by a scalar
(/:) :: (Real a) => MatT b -> a -> MatT b
(MatT a) /: denom
  = unsafePerformIO $ do
      withForeignPtr a $ \aa -> do
        mat_ptr <- c_divNum aa (CDouble (realToFrac denom))
        mat <- newForeignPtr cmatFree mat_ptr
        return (MatT mat)

-- |Matrix element-wise division
-- ToDo: Is this correct? the same type for a return value??
(/:/) :: MatT a -> MatT a -> MatT a
(MatT a) /:/ (MatT b) 
  = unsafePerformIO $ do
      withForeignPtr a $ \aa -> do
        withForeignPtr b $ \bb -> do
          mat_ptr <- c_divMat aa bb
          mat <- newForeignPtr cmatFree mat_ptr
          return (MatT mat)

-- Phantom types conversion
convAny :: MatT a -> MatT b
convAny (MatT m) = MatT m

-- blend :: Mat -> Mat -> Mat
-- blend (Mat a) (Mat b) = Mat $ fromIntegral $ c_addMat (fromIntegral a) (fromIntegral b)

monoColor :: (Channel a) => a -> Int -> Int -> RGB -> MatT b
monoColor _ h w (RGB r g b)
  = unsafePerformIO $ do
      mat_ptr <- c_monoColor (ci w) (ci h) (ci b) (ci g) (ci r)
      mat <- newForeignPtr cmatFree mat_ptr
      return (MatT mat)

showMatT :: MatT a -> IO ()
showMatT (MatT m) = do
  withForeignPtr m $ \mm -> do
    c_showMat mm

showMat :: Mat -> IO ()
showMat (Mat m) = do
  withForeignPtr m $ \mm -> do
    c_showMat mm

matToMatT :: Mat -> MatT a
matToMatT (Mat a) = MatT a

-- Color conversion

-- addWeighted :: Mat -> Double -> Mat -> Double -> Double -> Mat
-- addWeighted (Mat ma) alpha (Mat mb) beta gamma = fromId $ c_addWeighted (ci ma) (cd alpha) (ci mb) (cd beta) (cd gamma)

-- Image operations

readImg :: FilePath -> IO (MatT AnyPixel)
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

class CvtDepth from to where
  cvtDepth :: from -> to

instance CvtDepth (MatT CV_8UC3) (MatT CV_8UC1) where
  cvtDepth mat = cvtDepth' CV_8UC1 mat

cvtDepth' :: MatType -> MatT a -> MatT b
cvtDepth' t (MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    mat_ptr <- c_convertTo (unCMatType $ fromMatType t) mm
    mat <- newForeignPtr cmatFree mat_ptr 
    return $ MatT mat

cvtColor :: ConvertCode -> MatT a -> MatT b
cvtColor (ConvertCode code) (MatT m)
  = unsafePerformIO $ do
      withForeignPtr m $ \mm -> do
        mat_ptr <- c_cvtColor code mm
        mat <- newForeignPtr cmatFree mat_ptr
        return (MatT mat)

class Gray a
instance Gray CV_8UC1

class ToGray a b where
  toGray :: (Gray b) => MatT a -> MatT b

instance ToGray CV_8UC3 CV_8UC1 where
  toGray mat = cvtColor rgbToGray mat

instance ToGray AnyPixel CV_8UC1 where
  toGray mat
    = case matType mat of
        CV_8UC3 -> cvtColor rgbToGray mat
        _ -> error "Not supported yet"

{-
pixelAt  :: Int -> Int -> MatT a -> PixelT a
pixelAt y x (MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    scalar_ptr <- c_pixelAt (ci y) (ci x) mm
    mat <- newForeignPtr cmatFree mat_ptr
    return (Mat mat)

-}


{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module CV.Convert (
  bgrToGray
  , rgbToGray
  , luvToBGR
  , cvtDepth
  , cvtColor
  , convert
) where

import CV.Core
import CV.Types
import CV.FFI

import System.IO.Unsafe (unsafePerformIO)
import Foreign.ForeignPtr.Safe (withForeignPtr)
import Foreign.ForeignPtr (ForeignPtr,newForeignPtr)
import Foreign.C -- get the C types


-- Image color conversion
--

-- Code is listed in /usr/local/include/opencv2/imgproc/types_c.h
newtype ConvertCode = ConvertCode {unConvertCode :: CInt}
-- data ConvertCode = ConvertCode CInt
bgrToGray = ConvertCode (ci 6)
rgbToGray = ConvertCode (ci 7)
grayToBGR = ConvertCode (ci 8)
luvToBGR = ConvertCode (ci 58)

newtype CDepth = CDepth {unDepth :: CInt}
d_u8 = CDepth 0
d_s8 = CDepth 1
d_u16 = CDepth 2
d_s16 = CDepth 3
d_s32 = CDepth 4
d_f32 = CDepth 5
d_f64 = CDepth 6

cvtDepth' :: CDepth -> MatT a b -> MatT c b
cvtDepth' depth (MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    mat_ptr <- c_changeDepth (unDepth depth) mm
    mat <- newForeignPtr cmatFree mat_ptr 
    return $ MatT mat

cvtColor' :: ConvertCode -> MatT a b -> MatT a c
cvtColor' (ConvertCode code) (MatT m)
  = unsafePerformIO $ do
      withForeignPtr m $ \mm -> do
        mat_ptr <- c_cvtColor code mm
        mat <- newForeignPtr cmatFree mat_ptr
        return (MatT mat)

-- |Conversion of depth
class CvtDepth from to where
  cvtDepth :: MatT from a -> MatT to a

-- |Conversion of channels, keep depth (a)
class CvtColor from to where
  cvtColor :: MatT a from -> MatT a to

-- |Automatic conversion of depth and color. This may cause a runtime error when used with 'MatT' 'AnyChannel' b.
convert :: (CvtDepth a c, CvtColor b d) => MatT a b -> MatT c d
convert = cvtColor . cvtDepth


-- Conversion of depth. It only depends on the dest Mat, so there are only 7 kinds.
instance CvtDepth a U8 where
  cvtDepth = cvtDepth' d_u8

instance CvtDepth a S8 where
  cvtDepth = cvtDepth' d_s8

instance CvtDepth a U16 where
  cvtDepth = cvtDepth' d_u16

instance CvtDepth a S16 where
  cvtDepth = cvtDepth' d_s16

instance CvtDepth a S32 where
  cvtDepth = cvtDepth' d_s32

instance CvtDepth a F32 where
  cvtDepth = cvtDepth' d_f32

instance CvtDepth a F64 where
  cvtDepth = cvtDepth' d_f64


-- Conversion of colors/channels.
instance CvtColor C3BGR C1 where
  cvtColor = cvtColor' bgrToGray

instance CvtColor C3Luv C3BGR where
  cvtColor = cvtColor' luvToBGR

-- ... ToDo: Many more color conversion instances (ToDo: is there any good way to avoid listing all combinations?)
-- Maybe I should use Template Haskell.

-- Special case: unknown src type. (ToDo: a should be AnyChannel, or can I just leave it polymorphic?)
instance CvtColor AnyChannel C1 where
  cvtColor mat@(MatT m) = case numChannels mat of
                3 -> cvtColor' bgrToGray mat   -- Assuming BGR. since AnyChannel is returned only from readImg.
                1 -> (MatT m) :: MatT a C1 
                _ -> error "Only C1 or C3 can be converted to C1"
instance CvtColor AnyChannel C3 where
  cvtColor mat@(MatT m) = case numChannels mat of
                3 -> (MatT m) :: MatT a C3  -- Assuming BGR. since AnyChannel is returned only from readImg.
                1 -> cvtColor' grayToBGR mat
                _ -> error "Only C1 or C3 can be converted to C3"



-- |Phantom types conversion  Not type safe!!!
forceCast :: MatT a b -> MatT c d
forceCast (MatT m) = MatT m


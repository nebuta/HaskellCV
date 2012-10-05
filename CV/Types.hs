{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module CV.Types where

import Foreign.ForeignPtr (ForeignPtr,newForeignPtr)
import Foreign.ForeignPtr.Safe (withForeignPtr)
import Foreign.C -- get the C types
import System.IO.Unsafe (unsafePerformIO)


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

data CMat -- Mat type in C++ OpenCV
data CScalar -- Scalar type in C++ OpenCV

data CFilterEngine
data FilterEngine = FilterEngine !(ForeignPtr CFilterEngine)

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


type Angle = Double

data RGB = RGB Int Int Int

class Pixel a b where
  type PixelType a b :: *
  pixelAt :: Int -> Int -> MatT a b -> PixelType a b
  pixels :: MatT a b -> [[PixelType a b]]
  percentile :: Double -> MatT a b -> PixelType a b

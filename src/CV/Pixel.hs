{-# LANGUAGE MultiParamTypeClasses, TypeFamilies,TypeSynonymInstances #-}

module CV.Pixel where

-- This module should not be used from user.

import CV.Core
import CV.Types
import CV.FFI

import System.IO.Unsafe (unsafePerformIO)
import Foreign.ForeignPtr.Safe (withForeignPtr)
import Foreign.Marshal.Array (peekArray,advancePtr)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr
import Foreign.Storable

import Data.Word
import Data.Int

--
-- Get pixel(s)
--

-- ToDo: Design multi-channel support 

instance Pixel U8 C1 where
  type PixelType U8 C1 = Word8
  type PixelType' U8 C1 = Integer
  pixels = pixelsU8
  pixelAt y x = fromIntegral . pixelAtInt y x-- Stub: this conversion makes slow.
  percentile = percentileGeneral c_percentileInt fromIntegral

instance Pixel S8 C1 where
  type PixelType S8 C1 = Int8
  type PixelType' S8 C1 = Integer
  pixels = pixelsS8
  pixelAt y x = fromIntegral . pixelAtInt y x
  percentile = percentileGeneral c_percentileInt fromIntegral

instance Pixel U16 C1 where
  type PixelType U16 C1 = Word16
  pixels = pixelsU16
  pixelAt y x = fromIntegral . pixelAtInt y x
  percentile = percentileGeneral c_percentileInt fromIntegral

instance Pixel S16 C1 where
  type PixelType S16 C1 = Int16
  pixels = pixelsS16
  pixelAt y x = fromIntegral . pixelAtInt y x
  percentile = percentileGeneral c_percentileInt fromIntegral

instance Pixel S32 C1 where
  type PixelType S32 C1 = Int32
  pixels = pixelsS32
  pixelAt y x = fromIntegral . pixelAtInt y x
  percentile = percentileGeneral c_percentileInt fromIntegral

instance Pixel F32 C1 where
  type PixelType F32 C1 = Float
  pixels = pixelsF32
  pixelAt y x = pixelAtFloat y x
  percentile = percentileGeneral c_percentileFloat realToFrac

instance Pixel F64 C1 where
  type PixelType F64 C1 = Double
  pixels = pixelsF64
  pixelAt y x = pixelAtDouble y x
  percentile = percentileGeneral c_percentileFloat realToFrac

-- More instances for C2, C3, etc.

--ToDo: check if this is correct.

pixelsGeneral :: (Storable c) => (Ptr CMat -> IO (Ptr (Ptr c))) -> (c->PixelType a b) -> MatT a b -> [[PixelType a b]]
pixelsGeneral func conv mat@(MatT m) = 
        unsafePerformIO $ do
          withForeignPtr m $ \mm -> do
            let nr = (numRows mat)
            let nc = (numCols mat)
            pp <- func mm 
            row_ptrs <- peekArray nr pp
            val <- mapM (peekArray nc) row_ptrs
            free pp  --ToDo: Is this good?
            return $ map (map conv) val

-- pixelsS8 :: MatT S8 C1 -> [[PixelType S8 C1]]   --Single channel
pixelsU8 = pixelsGeneral c_valsU8 fromIntegral
pixelsS8 = pixelsGeneral c_valsS8 fromIntegral
pixelsU16 = pixelsGeneral c_valsU16 fromIntegral
pixelsS16 = pixelsGeneral c_valsS16 fromIntegral
pixelsS32 = pixelsGeneral c_valsS32 fromIntegral
pixelsF32 = pixelsGeneral c_valsF32 realToFrac
pixelsF64 = pixelsGeneral c_valsF64 realToFrac


percentileGeneral func conv perc (MatT m)
  = unsafePerformIO $ do
      withForeignPtr m $ \mm -> do
        val <- func (cd perc) mm
        return (conv val)

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

{-
pixelsU16 :: (ChannelC1 b) => MatT U16 b -> [[Word16]]   --Single channel
pixelsU16 mat@(MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    let nr = (numRows mat)
    let nc = (numCols mat)
    pp <- c_valsU16 mm   
    row_ptrs <- peekArray nr pp
    val <- mapM (peekArray nc) row_ptrs
    free pp  --ToDo: Is this good?
    return $ map (map fromIntegral) val


pixelsS16 :: (ChannelC1 b) => MatT S16 b -> [[Int16]]   --Single channel
pixelsS16 mat@(MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    let nr = (numRows mat)
    let nc = (numCols mat)
    pp <- c_valsS16 mm  
    row_ptrs <- peekArray nr pp
    val <- mapM (peekArray nc) row_ptrs
    free pp  --ToDo: Is this good?
    return $ map (map fromIntegral) val

pixelsS32 :: (ChannelC1 b) => MatT S32 b -> [[Int32]]   --Single channel
pixelsS32 mat@(MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    let nr = (numRows mat)
    let nc = (numCols mat)
    pp <- c_valsS32 mm 
    row_ptrs <- peekArray nr pp
    val <- mapM (peekArray nc) row_ptrs
    free pp  --ToDo: Is this good?
    return $ map (map fromIntegral) val

pixelsF32 :: (ChannelC1 b) => MatT F32 b -> [[Float]]   --Single channel
pixelsF32 mat@(MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    let nr = (numRows mat)
    let nc = (numCols mat)
    pp <- c_valsF32 mm
    row_ptrs <- peekArray nr pp
    val <- mapM (peekArray nc) row_ptrs
    free pp  --ToDo: Is this good?
    return (map (map realToFrac) val)


pixelsF64 :: (ChannelC1 b) => MatT F64 b -> [[Double]]   --Single channel
pixelsF64 mat@(MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    let nr = (numRows mat)
    let nc = (numCols mat)
    pp <- c_valsF64 mm
    row_ptrs <- peekArray nr pp
    val <- mapM (peekArray nc) row_ptrs
    free pp  --ToDo: Is this good?
    return (map (map realToFrac) val)
-}

--Single channel
pixelAtInt  :: Int -> Int -> MatT a b -> Int
pixelAtInt y x (MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    val <- c_pixelIntAt (ci y) (ci x) mm
    return (fromIntegral val)

  --Single channel
pixelAtFloat :: Int -> Int -> MatT a b -> Float
pixelAtFloat y x (MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    val <- c_pixelFloatAt (ci y) (ci x) mm
    return (realToFrac val)

  --Single channel
pixelAtDouble :: Int -> Int -> MatT a b -> Double
pixelAtDouble y x (MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    val <- c_pixelDoubleAt (ci y) (ci x) mm
    return (realToFrac val)


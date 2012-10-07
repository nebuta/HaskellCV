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


--
-- Get pixel(s)
--

-- ToDo: Design multi-channel support 



instance Pixel U8 C1 where
  type PixelType U8 C1 = Int
  pixels = pixelsU8
  pixelAt = pixelAtInt
  percentile = percentileInt

instance Pixel S8 C1 where
  type PixelType S8 C1 = Int
  pixels = pixelsS8
  pixelAt = pixelAtInt
  percentile = percentileInt

instance Pixel U16 C1 where
  type PixelType U16 C1 = Int
  pixels = pixelsU16
  pixelAt = pixelAtInt
  percentile = percentileInt

instance Pixel S16 C1 where
  type PixelType S16 C1 = Int
  pixels = pixelsS16
  pixelAt = pixelAtInt
  percentile = percentileInt

instance Pixel S32 C1 where
  type PixelType S32 C1 = Int
  pixels = pixelsS32
  pixelAt = pixelAtInt
  percentile = percentileInt

instance Pixel F32 C1 where
  type PixelType F32 C1 = Float
  pixels = pixelsF32
  pixelAt = pixelAtFloat
  percentile p = realToFrac . percentileFloat p

instance Pixel F64 C1 where
  type PixelType F64 C1 = Double
  pixels = pixelsF64
  pixelAt = pixelAtDouble
  percentile = percentileFloat

--ToDo: check if this is correct.
pixelsU8 :: (ChannelC1 b) => MatT U8 b -> [[Int]]   --Single channel
pixelsU8 mat@(MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    let nr = (numRows mat)
    let nc = (numCols mat)
    pp <- c_valsU8 mm    --Only uchar!!!!!
    row_ptrs <- peekArray nr pp
    val <- mapM (peekArray nc) row_ptrs
    free pp  --ToDo: Is this good?
    return $ map (map fromIntegral) val

pixelsS8 :: (ChannelC1 b) => MatT S8 b -> [[Int]]   --Single channel
pixelsS8 mat@(MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    let nr = (numRows mat)
    let nc = (numCols mat)
    pp <- c_valsS8 mm 
    row_ptrs <- peekArray nr pp
    val <- mapM (peekArray nc) row_ptrs
    free pp  --ToDo: Is this good?
    return $ map (map fromIntegral) val

pixelsU16 :: (ChannelC1 b) => MatT U16 b -> [[Int]]   --Single channel
pixelsU16 mat@(MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    let nr = (numRows mat)
    let nc = (numCols mat)
    pp <- c_valsU16 mm   
    row_ptrs <- peekArray nr pp
    val <- mapM (peekArray nc) row_ptrs
    free pp  --ToDo: Is this good?
    return $ map (map fromIntegral) val

pixelsS16 :: (ChannelC1 b) => MatT S16 b -> [[Int]]   --Single channel
pixelsS16 mat@(MatT m) = unsafePerformIO $ do
  withForeignPtr m $ \mm -> do
    let nr = (numRows mat)
    let nc = (numCols mat)
    pp <- c_valsS16 mm  
    row_ptrs <- peekArray nr pp
    val <- mapM (peekArray nc) row_ptrs
    free pp  --ToDo: Is this good?
    return $ map (map fromIntegral) val

pixelsS32 :: (ChannelC1 b) => MatT S32 b -> [[Int]]   --Single channel
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


{-# LANGUAGE MultiParamTypeClasses,TypeSynonymInstances #-}

module CV.Instance where

import CV.Types
import CV.Core
import CV.FFI
import Foreign.C -- get the C types

import Foreign.ForeignPtr (ForeignPtr,newForeignPtr)
import Data.Word
import System.IO.Unsafe (unsafePerformIO)

-- Used through randMat in RandMat class
randMat' :: CInt -> Int -> Int -> IO (MatT a b)
randMat' t y x = do
  mat_ptr <- c_randMat t (ci y) (ci x) 0 0
  mat <- newForeignPtr cmatFree mat_ptr
  return (MatT mat)

randMatFromKey' :: CInt -> Word8 -> Int -> Int -> MatT a b
randMatFromKey' t key y x = unsafePerformIO $ do
  mat_ptr <- c_randMat t (ci y) (ci x) 1 (fromIntegral key)
  mat <- newForeignPtr cmatFree mat_ptr
  return (MatT mat)


instance RandMat U8 C1 where
  randMat y x = randMat' (unCMatType cv_8UC1) y x
  randMatFromKey k y x = randMatFromKey' (unCMatType cv_8UC1) k y x

instance RandMat S8 C1 where
  randMat y x = randMat' (unCMatType cv_8SC1) y x

instance RandMat U16 C1 where
  randMat y x = randMat' (unCMatType cv_16UC1) y x

instance RandMat S16 C1 where
  randMat y x = randMat' (unCMatType cv_16SC1) y x

instance RandMat S32 C1 where
  randMat y x = randMat' (unCMatType cv_32SC1) y x

instance RandMat F32 C1 where
  randMat y x = randMat' (unCMatType cv_32FC1) y x

instance RandMat F64 C1 where
  randMat y x = randMat' (unCMatType cv_64FC1) y x


-- ToDo: Many more combination


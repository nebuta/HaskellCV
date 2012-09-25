{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module CV.FFI where

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr (ForeignPtr)

data CMat -- Mat type in C++ OpenCV
data Mat = Mat !(ForeignPtr CMat)

type MatId = CInt

-- Matrix basic operations

foreign import ccall unsafe "matTest" c_matTest :: IO ()
foreign import ccall "zeros1D" c_zeros :: CInt -> CInt
-- foreign import ccall "valAt" c_valAt :: CInt -> CInt -> CInt
foreign import ccall "length" c_length :: CInt -> CInt
foreign import ccall "cellDetectTest" c_cellDetectTest :: IO ()
foreign import ccall "maxDetectTest" c_maxDetectTest :: IO ()
foreign import ccall "randMat" c_randMat :: CInt -> CInt -> CInt
foreign import ccall "showMat" c_showMat :: Ptr CMat -> IO ()
foreign import ccall "monoColor" c_monoColor :: CInt -> CInt -> CInt -> CInt -> CInt -> IO (Ptr CMat)

foreign import ccall "add" c_addMat :: Ptr CMat -> Ptr CMat -> IO (Ptr CMat)
foreign import ccall "subMat" c_subMat :: Ptr CMat -> Ptr CMat -> IO (Ptr CMat)
foreign import ccall "eqMat" c_eqMat :: Ptr CMat -> Ptr CMat -> IO CInt

foreign import ccall "addWeighted" c_addWeighted :: MatId -> CDouble -> MatId -> CDouble -> CDouble -> MatId
foreign import ccall "abs" c_abs :: Ptr CMat -> IO (Ptr CMat) 

-- image operations
foreign import ccall "readImg" c_readImg :: CString -> IO (Ptr CMat)
foreign import ccall "cvtColor" c_cvtColor :: CInt -> CInt -> CInt

-- Filters

foreign import ccall "gaussian" c_gaussian :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt



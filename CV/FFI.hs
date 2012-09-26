{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module CV.FFI where

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr (ForeignPtr)

data CMat -- Mat type in C++ OpenCV
data Mat = Mat !(ForeignPtr CMat)

type MatId = CInt

-- Matrix basic operations

foreign import ccall "randMat" c_randMat :: CInt -> CInt -> IO (Ptr CMat)
foreign import ccall "showMat" c_showMat :: Ptr CMat -> IO ()
foreign import ccall "monoColor" c_monoColor :: CInt -> CInt -> CInt -> CInt -> CInt -> IO (Ptr CMat)

foreign import ccall "m_add" c_addMat :: Ptr CMat -> Ptr CMat -> IO (Ptr CMat)
foreign import ccall "m_sub" c_subMat :: Ptr CMat -> Ptr CMat -> IO (Ptr CMat)
foreign import ccall "m_eq" c_eqMat :: Ptr CMat -> Ptr CMat -> IO CInt

foreign import ccall "addWeighted" c_addWeighted :: Ptr CMat -> CDouble -> Ptr CMat -> CDouble -> CDouble -> IO (Ptr CMat)
foreign import ccall "m_abs" c_abs :: Ptr CMat -> IO (Ptr CMat) 

-- image operations
foreign import ccall "readImg" c_readImg :: CString -> IO (Ptr CMat)
foreign import ccall "cvtColor" c_cvtColor :: CInt -> Ptr CMat -> IO (Ptr CMat)

-- Filters

foreign import ccall "gaussian" c_gaussian :: CInt -> CInt -> CInt -> CInt -> Ptr CMat -> IO (Ptr CMat)



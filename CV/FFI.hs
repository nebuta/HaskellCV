{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module CV.FFI where

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr (ForeignPtr)

data CMat -- Mat type in C++ OpenCV
data Mat = Mat !(ForeignPtr CMat)

foreign import ccall "&matFree" cmatFree :: FunPtr(Ptr CMat->IO())

-- Matrix basic operations
{-
foreign import ccall unsafe "m_row" c_row :: Ptr CMat -> CInt -> IO (Ptr CMat)
foreign import ccall unsafe "m_col" c_col :: Ptr CMat -> CInt -> IO (Ptr CMat)
foreign import ccall unsafe "m_rowRange" c_rowRange :: Ptr CMat -> CInt -> CInt -> IO (Ptr CMat)
foreign import ccall unsafe "m_colRange" c_colRange :: Ptr CMat -> CInt -> CInt -> IO (Ptr CMat)

-- foreign import ccall unsafe "m_diag" c_diag :: Ptr CMat -> CInt -> IO (Ptr CMat)
-- foreign import ccall unsafe "m_copyTo" c_copyTo :: Ptr CMat -> Ptr CMat -> IO ()
-- Mat::convertTo
-- Mat::assignTo
-- Mat::setTo
foreign import ccall unsafe "m_setTo" c_setTo :: Ptr CMat -> Ptr CMat -> Ptr CMat -> IO ()

-- Mat::reshape
foreign import ccall unsafe "m_clone" c_clone :: Ptr CMat -> IO (Ptr CMat)
-}

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



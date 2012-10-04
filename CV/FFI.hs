{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module CV.FFI where

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr (ForeignPtr)

data CMat -- Mat type in C++ OpenCV
data CScalar -- Scalar type in C++ OpenCV

data CFilterEngine
data FilterEngine = FilterEngine !(ForeignPtr CFilterEngine)

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

foreign import ccall "m_pixelIntAt" c_pixelIntAt :: CInt -> CInt -> Ptr CMat -> IO Int

foreign import ccall "m_valsUChar" c_valsUChar :: Ptr CMat -> IO (Ptr (Ptr CUChar))

foreign import ccall "m_type" c_type :: Ptr CMat -> IO CInt
foreign import ccall "m_rows" c_rows :: Ptr CMat -> IO CInt
foreign import ccall "m_cols" c_cols :: Ptr CMat -> IO CInt


foreign import ccall "m_add" c_addMat :: Ptr CMat -> Ptr CMat -> IO (Ptr CMat)
foreign import ccall "m_sub" c_subMat :: Ptr CMat -> Ptr CMat -> IO (Ptr CMat)
foreign import ccall "m_mul" c_mulMat :: Ptr CMat -> Ptr CMat -> IO (Ptr CMat)
foreign import ccall "m_div" c_divMat :: Ptr CMat -> Ptr CMat -> IO (Ptr CMat)
foreign import ccall "m_divNum" c_divNum :: Ptr CMat -> CDouble -> IO (Ptr CMat)
foreign import ccall "m_eq" c_eqMat :: Ptr CMat -> Ptr CMat -> IO CInt
foreign import ccall "m_compare" c_compare :: Ptr CMat -> Ptr CMat -> CInt -> IO (Ptr CMat)

foreign import ccall "addWeighted" c_addWeighted :: Ptr CMat -> CDouble -> Ptr CMat -> CDouble -> CDouble -> IO (Ptr CMat)
foreign import ccall "m_abs" c_abs :: Ptr CMat -> IO (Ptr CMat) 

-- image operations
foreign import ccall "readImg" c_readImg :: CString -> IO (Ptr CMat)
foreign import ccall "cvtColor" c_cvtColor :: CInt -> Ptr CMat -> IO (Ptr CMat)
foreign import ccall "m_changeDepth" m_changeDepth :: CInt -> Ptr CMat -> IO (Ptr CMat)

-- Filters

foreign import ccall "f_gaussian" c_gaussian :: CInt -> CInt -> CDouble -> CDouble -> Ptr CMat -> IO (Ptr CMat)
foreign import ccall "f_boxFilter" c_boxFilter :: CInt -> CInt -> Ptr CMat -> IO (Ptr CMat)
foreign import ccall "f_derivFilter" c_derivFilter :: CInt -> CInt -> CInt -> Ptr CMat -> IO (Ptr CMat)
foreign import ccall "f_medianFilter" c_medianFilter :: CInt -> Ptr CMat -> IO (Ptr CMat)
foreign import ccall "f_laplacian" c_laplacian:: CInt -> CDouble -> CDouble -> Ptr CMat -> IO (Ptr CMat)
foreign import ccall "f_bilateral" c_bilateral :: CInt -> CDouble -> CDouble -> Ptr CMat -> IO (Ptr CMat)
foreign import ccall "f_sobel" c_sobel :: CInt -> CInt -> CInt -> CDouble -> CDouble -> Ptr CMat -> IO (Ptr CMat)


-- Filters using mask
foreign import ccall "f_getStructuringElement" c_getStructuringElement :: CInt -> CInt -> CInt -> IO (Ptr CMat)
foreign import ccall "f_dilate" c_dilate :: Ptr CMat -> Ptr CMat -> IO (Ptr CMat)
foreign import ccall "f_erode" c_erode :: Ptr CMat -> Ptr CMat -> IO (Ptr CMat)


-- foreign import ccall "f_createBoxFilter" c_gaussian :: CInt -> CInt -> CInt -> CInt -> Ptr CMat -> IO (Ptr CMat)


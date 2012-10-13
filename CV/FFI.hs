{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module CV.FFI where

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr (ForeignPtr)

import CV.Types

-- Mat create and destroy
foreign import ccall "m_newMat" c_newMat :: CInt -> CInt -> CInt -> IO (Ptr CMat)
foreign import ccall "m_randMat" c_randMat :: CInt -> CInt -> CInt -> CInt -> CUInt -> IO (Ptr CMat)
foreign import ccall "m_clone" c_clone :: Ptr CMat -> IO (Ptr CMat)
foreign import ccall "&matFree" cmatFree :: FunPtr(Ptr CMat->IO())
foreign import ccall "monoColor" c_monoColor :: CInt -> CInt -> CInt -> CInt -> CInt -> IO (Ptr CMat)

-- Image I/O
foreign import ccall "readImg" c_readImg :: CString -> IO (Ptr CMat)
foreign import ccall "showMat" c_showMat :: Ptr CMat -> CInt -> IO ()

-- Video I/O
foreign import ccall "m_newVideoWriter" c_newVideoWriter :: CString -> CString -> CDouble -> CInt -> CInt -> IO (Ptr CVideoWriter)
foreign import ccall "&videoWriterFree" cvideoWriterFree :: FunPtr(Ptr CVideoWriter->IO())
foreign import ccall "m_videoWrite" c_videoWrite:: Ptr CVideoWriter -> Ptr CMat -> IO ()

-- Get  value
foreign import ccall "m_pixelIntAt" c_pixelIntAt :: CInt -> CInt -> Ptr CMat -> IO CInt
foreign import ccall "m_pixelFloatAt" c_pixelFloatAt :: CInt -> CInt -> Ptr CMat -> IO CFloat
foreign import ccall "m_pixelDoubleAt" c_pixelDoubleAt :: CInt -> CInt -> Ptr CMat -> IO CDouble

foreign import ccall "m_valsU8" c_valsU8 :: Ptr CMat -> IO (Ptr (Ptr CUChar))
foreign import ccall "m_valsS8" c_valsS8 :: Ptr CMat -> IO (Ptr (Ptr CChar))
foreign import ccall "m_valsU16" c_valsU16 :: Ptr CMat -> IO (Ptr (Ptr CUShort))
foreign import ccall "m_valsS16" c_valsS16 :: Ptr CMat -> IO (Ptr (Ptr CShort))
foreign import ccall "m_valsS32" c_valsS32 :: Ptr CMat -> IO (Ptr (Ptr CInt))
foreign import ccall "m_valsF32" c_valsF32 :: Ptr CMat -> IO (Ptr (Ptr CFloat))
foreign import ccall "m_valsF64" c_valsF64 :: Ptr CMat -> IO (Ptr (Ptr CDouble))


foreign import ccall "m_type" c_type :: Ptr CMat -> IO CInt
foreign import ccall "m_channels" c_channels :: Ptr CMat -> IO CInt
foreign import ccall "m_rows" c_rows :: Ptr CMat -> IO CInt
foreign import ccall "m_cols" c_cols :: Ptr CMat -> IO CInt

foreign import ccall "m_hist" c_hist :: CInt -> CInt -> CFloat -> CFloat -> Ptr CMat -> IO (Ptr CInt) 

foreign import ccall "m_percentileInt" c_percentileInt :: CDouble -> Ptr CMat -> IO CInt 
foreign import ccall "m_percentileFloat" c_percentileFloat :: CDouble -> Ptr CMat -> IO CDouble

foreign import ccall "m_add" c_addMat :: Ptr CMat -> Ptr CMat -> IO (Ptr CMat)
foreign import ccall "m_sub" c_subMat :: Ptr CMat -> Ptr CMat -> IO (Ptr CMat)
foreign import ccall "m_mul" c_mulMat :: Ptr CMat -> Ptr CMat -> IO (Ptr CMat)
foreign import ccall "m_div" c_divMat :: Ptr CMat -> Ptr CMat -> IO (Ptr CMat)
foreign import ccall "m_divNum" c_divNum :: Ptr CMat -> CDouble -> IO (Ptr CMat)
foreign import ccall "m_eq" c_eqMat :: Ptr CMat -> Ptr CMat -> IO CInt


-- Search and histogram
foreign import ccall "m_compare" c_compare :: Ptr CMat -> Ptr CMat -> CInt -> IO (Ptr CMat)
foreign import ccall "m_findNonZero" c_findNonZero :: Ptr CMat -> IO (Ptr CInt)
foreign import ccall "regionalMax" c_regionalMax :: Ptr CMat -> IO (Ptr CMat)

foreign import ccall "addWeighted" c_addWeighted :: Ptr CMat -> CDouble -> Ptr CMat -> CDouble -> CDouble -> IO (Ptr CMat)
foreign import ccall "m_abs" c_abs :: Ptr CMat -> IO (Ptr CMat) 

-- image operations
foreign import ccall "cvtColor" c_cvtColor :: CInt -> Ptr CMat -> IO (Ptr CMat)
foreign import ccall "m_changeDepth" c_changeDepth :: CInt -> Ptr CMat -> IO (Ptr CMat)


-- Drawing

foreign import ccall "m_circle" c_circle :: Ptr CMat -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
foreign import ccall "m_rectangle" c_rectangle :: Ptr CMat -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
foreign import ccall "m_line" c_line :: Ptr CMat -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()


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


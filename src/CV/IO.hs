module CV.IO where

import CV.Core
import CV.Convert
import CV.Types
import CV.FFI


import Foreign.C -- get the C types
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr,newForeignPtr)
import Foreign.ForeignPtr.Safe (withForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree)
import System.IO.Unsafe (unsafePerformIO)

readImg :: FilePath -> IO AnyImage
    --ToDo: Consider if AnyDepth and AnyChannel is good, or I should use polymorphic types as a b
readImg path = do
  withCString path $ \cstr_path -> do
    mat_ptr <- c_readImg cstr_path
    mat <- newForeignPtr cmatFree mat_ptr 
    return (MatT mat)

{-
readImg' :: FilePath -> IO (MatT a b)
readImg' path = do
	img <- readImg' path
	return (convert img)-}
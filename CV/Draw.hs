module CV.Draw where

import CV.Core
import CV.Types
import CV.FFI

import Control.Monad (foldM)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.ForeignPtr.Safe (withForeignPtr)
import Foreign.Marshal.Array (peekArray,advancePtr)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr,newForeignPtr)


data Shape = Circle Coord Int RGB | Rect Coord Coord RGB | Line Coord Coord RGB

-- ToDo: Design a monad for destructive update like MArray
-- Outside the monad, referential transparency should be maintained.
--

draw :: MatT a b -> [Shape] -> MatT a b
draw (MatT m) shapes = unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      mat_ptr <- c_clone mm
      foldM drawOne mat_ptr shapes
      mat <- newForeignPtr cmatFree mat_ptr     
      return (MatT mat)

-- drawOne is destructive.
drawOne :: Ptr CMat -> Shape -> IO (Ptr CMat)
drawOne mm (Circle (Coord y x) radius (RGB r g b)) = do
  c_circle mm (ci y) (ci x) (ci radius) (ci b) (ci g) (ci r) 
  return mm


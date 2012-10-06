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

type Radius = Int
type Thickness = Int
data Shape = Circle Coord Radius RGB Thickness
              | Rect Coord Coord RGB Thickness
              | Line Coord Coord RGB Thickness

-- ToDo: Design a monad for destructive update like MArray
-- Outside the monad, referential transparency should be maintained.
--

draw :: MatT a b -> [Shape] -> MatT a b
draw (MatT m) shapes = unsafePerformIO $ do
    withForeignPtr m $ \mm -> do
      mat_ptr <- c_clone mm
      sequence_ (map (drawOne mat_ptr) shapes)
      mat <- newForeignPtr cmatFree mat_ptr     
      return (MatT mat)

-- drawOne is destructive for Mat pointed by Ptr CMat
drawOne :: Ptr CMat -> Shape -> IO ()
drawOne mm (Circle (Coord y x) radius (RGB r g b) thickness) = 
  c_circle mm (ci y) (ci x) (ci radius) (ci b) (ci g) (ci r) (ci thickness)

drawOne mm (Rect (Coord y1 x1) (Coord y2 x2) (RGB r g b) thickness) =
  c_rectangle mm (ci y1) (ci x1) (ci y2) (ci x2) (ci b) (ci g) (ci r) (ci thickness)

drawOne mm (Line (Coord y1 x1) (Coord y2 x2) (RGB r g b) thickness) =
  c_line mm (ci y1) (ci x1) (ci y2) (ci x2) (ci b) (ci g) (ci r) (ci thickness)

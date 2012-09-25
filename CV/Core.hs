-- Core.hs

{-# LANGUAGE ForeignFunctionInterface #-}

module CV.Core where

import Foreign.C -- get the C types
import CV.FFI
import Data.ByteString hiding (map)
import Data.String

data Mat = Mat Int    -- has an int ID.
      deriving (Show)

instance Eq Mat where
  (Mat a) == (Mat b) =  a == b || c_eqMat (fromIntegral a) (fromIntegral b) /= (fromIntegral 0)


-- MatId is CInt
fromId :: MatId -> Mat
fromId = Mat . fromIntegral

data Bits = B32 | B16 | B8
data Channel = Channel Bits

data Size = Mat1D Int | Mat2D Int Int | Mat3D Int Int Int
data RGB = RGB Int Int Int

yellow, red, blue, green :: RGB
yellow = RGB 255 255 0
red = RGB 255 0 0
blue = RGB 0 0 255
green = RGB 0 255 0


-- Matrix operations

-- |Element-wise absolute value
cvAbs :: Mat -> Mat
cvAbs (Mat m) = fromId $ c_abs (ci m)

-- |Matrix addition
(+:+) :: Mat -> Mat -> Mat
(Mat a) +:+ (Mat b) = (Mat . fromIntegral) $ c_addMat (fromIntegral a) (fromIntegral b)

-- |Matrix subtraction
(-:-) :: Mat -> Mat -> Mat
(Mat a) -:- (Mat b) = (Mat . fromIntegral) $ c_subMat (fromIntegral a) (fromIntegral b)



-- blend :: Mat -> Mat -> Mat
-- blend (Mat a) (Mat b) = Mat $ fromIntegral $ c_addMat (fromIntegral a) (fromIntegral b)

matId :: Mat -> Int
matId (Mat a) = a

monoColor :: Channel -> Int -> Int -> RGB -> Mat
monoColor c h w (RGB r g b) = Mat (fromIntegral $ c_monoColor (fromIntegral w) (fromIntegral h)
                              (fromIntegral b) (fromIntegral g) (fromIntegral r))

showMat :: Mat -> IO ()
showMat (Mat id) = c_showMat (fromIntegral id)

matsize :: Mat -> Int
matsize (Mat id) = fromIntegral (c_length (fromIntegral id))


-- Random Matrix with a specified size
randMat :: Int -> Int -> Mat
randMat y x = fromId (c_randMat (fromIntegral y) (fromIntegral x))

-- 1D vector with 0 norm
zeros :: Int -> Mat
zeros n = fromId (c_zeros (fromIntegral n))

vals :: Mat -> [Int]
vals (Mat id) = map (fromIntegral . c_valAt (fromIntegral id)) (map fromIntegral [0..len-1])
  where
    len :: Int
    len = fromIntegral (c_length (fromIntegral id))

-- 2D Filters
--


-- Default for gaussian
gauss :: Int -> Mat -> Mat
gauss sigma mat = gaussian 0 0 sigma 0 mat

-- Full feature gaussian
gaussian :: Int -> Int -> Int -> Int -> Mat -> Mat
gaussian kw kh sx sy (Mat a) = Mat $ fromIntegral $ c_gaussian (ci kw) (ci kh) (ci sx) (ci sy) (ci a)

ci :: Int -> CInt
ci = fromIntegral

cd :: Double -> CDouble
cd = realToFrac

-- Image operations
readImg :: FilePath -> IO Mat
readImg file = do
  withCString file $ \cstr_path -> do
    mat <- c_readImg cstr_path
    return (Mat (fromIntegral mat))

-- Color conversion
newtype ConvertCode = ConvertCode {unConvertCode :: CInt}

rgbToGray = ConvertCode (ci 7)

addWeighted :: Mat -> Double -> Mat -> Double -> Double -> Mat
addWeighted (Mat ma) alpha (Mat mb) beta gamma = fromId $ c_addWeighted (ci ma) (cd alpha) (ci mb) (cd beta) (cd gamma)

cvtColor :: ConvertCode -> Mat -> Mat
cvtColor (ConvertCode code) (Mat a) = fromId $ c_cvtColor code (ci a)



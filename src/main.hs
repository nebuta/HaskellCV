module Main where

import CV
import CV.Demo
import CV.Draw hiding (Rect)

import Data.List (sort,sortBy)
import Debug.Trace (trace,traceShow)
import Data.Word (Word8)
import Data.Function 

import Debug.Trace (trace)
import Control.Concurrent (forkIO)

import System.IO
import Control.Monad
import Control.Applicative

import Text.Printf

findParticles :: GrayImage -> IO [Pos]
findParticles img = do
  let filtered = prefilter img
  showMatT $ imadjust 0 100 0 100 (filtered :: GrayImage)
  return (detect filtered)
  where prefilter = id -- gauss (fromIntegral 9)

minDist = 3 :: Int

detect :: GrayImage -> [Pos]
detect img = (refinePos .
              filterDist (fromIntegral minDist) .
              filterInt perc .
              map (addInt img) .
              findNonZero .
              regionalMax)
                $ img
  where
    perc = 20.0   -- in %
    minInt = fromIntegral $ percentile perc img
    dilated = dilate (fromStrEl (Ellipse minDist minDist)) img

addInt :: GrayImage -> Coord -> (Coord,Word8)
addInt img c@(Coord y x) = (c,pixelAt y x img)

findIndexMat :: CmpFun a b -> MatT a b -> MatT a b -> [Coord]
findIndexMat f a b = findNonZero $ CV.compare f a b

refinePos :: [(Coord,Word8)] -> [Pos]
refinePos cs = map f cs
  where f (Coord y x,_) = Pos (fromIntegral y) (fromIntegral x)--Stub

filterInt :: Double -> Iso [(Coord,Word8)]
filterInt perc ps = filter (\(c,i) -> i >= thres) ps
  where
    thres = snd $ (reverse $ sortBy (Prelude.compare `on` snd) ps) !! idx
 --   idx = min (length ps - 1) 20
    idx = min (length ps - 1) (floor (perc / 100.0 * fromIntegral (length ps)))

filterDist :: Double -> Iso [(Coord,Word8)]
filterDist dist ps = filter (f ps dist) ps
  where
    f :: [(Coord,Word8)] -> Double -> (Coord,Word8) -> Bool
    f ps dist p@(_,int) = all (\(_, intOther) -> intOther <= int) $ filter (adjacent dist p) ps
    adjacent :: Double -> (Coord,Word8) -> (Coord,Word8) -> Bool
    adjacent d (c1,_) (c2,_) = let dd = distSq c1 c2 in dd > 0.1 && dd < d*d
    distSq :: Coord -> Coord -> Double
    distSq (Coord y1 x1) (Coord y2 x2) = fromIntegral ( (y1-y2)*(y1-y2) + (x1-x2)*(x1-x2) )
{-
data Traj = Traj [Pos3D]

connectTraj :: [[Pos3D]] -> [Traj]
connectTraj ps = [Traj []] --stub

getTraj :: [GrayImage] -> [Traj]
getTraj imgs = connectTraj . addFrameIdx . map findParticles $ imgs
-}
addFrameIdx :: [[Pos]] -> [[Pos3D]]
addFrameIdx pss = zipWith f [0..length pss-1] pss
  where
    f i ps = map (g i) ps
    g i (Pos x y) = Pos3D i x y

filename :: Int -> String
filename n = rootfolder ++ "img/cell"++printf "%03d" n++".jpg"

rootfolder = "/Users/hiroyuki/Documents/Groves/Nanodots plus CD80/"

main :: IO ()
main = do
  h <- openFile (rootfolder++"filelist.txt") ReadMode
  ne <- read <$> hGetLine h
  entries <- replicateM ne $ readEntry h
  mapM_ processEntry entries
  return ()

readEntry :: Handle -> IO ([String],[[Double]])
readEntry h = do
  fs <- replicateM 3 $ hGetLine h
  nl <- read <$> hGetLine h
  ls <- replicateM nl $ hGetLine h
  return (fs, map (map read . words) ls)

processEntry :: ([FilePath],[[Double]]) -> IO ()
processEntry ([f561,f488,fricm],pss) = do
  img561 <- readImg f561
  img488 <- readImg f488
  imgRICM <- readImg fricm
  putStrLn $ "File: " ++ f561
  mapM_ (processCropped img561 img488 imgRICM) pss

processCropped :: AnyImage -> AnyImage -> AnyImage -> [Double] -> IO ()
processCropped all561 green ricm [cx,cy,r] = do
  let x = round (cx-r)
  let y = round (cy-r)
  let w = round (r*2)
  let ps561 = []
  let ps488 = []
  let img561 = crop all561 x y w w
  let img488 = crop green x y w w
  ps561 <- findParticles (convert img561 :: GrayImage)
  ps488 <- findParticles (convert img488 :: GrayImage)
  print $ length ps561
  print $ length ps488
    -- Type annotation is NOT necessary. GrayImage = MatT U8 C1, defined in CV.Types
  let cs561 = map roundPos ps561
  let cs488 = map roundPos ps488
  let out561 = draw (convert img561) (map (\x-> Circle x 1 red (-1)) cs561) :: MatT U8 C3
--  let out488 = draw (convert img488) (map (\x-> Circle x 1 red (-1)) cs488) :: MatT U8 C1
  -- showMatT (img561 :: MatT U8 C3)
--  showMatT out488
  return ()

roundPos :: Pos -> Coord
roundPos (Pos y x) = Coord (round y) (round x)

demos :: IO ()
demos = do
  demo8

module Main where

import CV
import CV.Filter
import CV.Demo
import CV.Draw

import Data.List (sort,sortBy)
import Debug.Trace (trace,traceShow)
import Data.Word (Word8)
import Data.Function 

import Debug.Trace (trace)
import Control.Concurrent (forkIO)

import Text.Printf

findParticles :: GrayImage -> [Pos]
findParticles = detect . prefilter
  where prefilter = gauss (fromIntegral 5)

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
    thres = snd $ (reverse $ sortBy (Prelude.compare `on` snd) ps) !! (trace (show idx) idx)
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


data Traj = Traj [Pos3D]

connectTraj :: [[Pos3D]] -> [Traj]
connectTraj ps = [Traj []] --stub

getTraj :: [GrayImage] -> [Traj]
getTraj imgs = connectTraj . addFrameIdx . map findParticles $ imgs

addFrameIdx :: [[Pos]] -> [[Pos3D]]
addFrameIdx pss = zipWith f [0..length pss-1] pss
  where
    f i ps = map (g i) ps
    g i (Pos x y) = Pos3D i x y

filename :: Int -> String
filename n = rootfolder ++ "img/cell"++printf "%03d" n++".jpg"

rootfolder = "~/Downloads/examples/"

main :: IO ()
main = do
  first <- readImg $ filename 1
  vw <- newVideo (rootfolder ++ "out.mpg") MPEG1 30 (numRows first) (numCols first)
  pss <- mapM_ (processOneFrame vw) [1..600]
  return ()

processOneFrame :: VideoWriter -> Int -> IO [Pos]
processOneFrame vw n = do 
  img <- readImg (filename n)
  putStrLn $ "Frame: " ++ show n
  let ps = findParticles (convert img :: GrayImage)
    -- Type annotation is NOT necessary. GrayImage = MatT U8 C1, defined in CV.Types
  let coords = map roundPos ps
  let out = draw img (map (\x-> Circle x 1 red (-1)) coords)
  addFrame vw out
  print (n,(length ps))
  return ps

roundPos :: Pos -> Coord
roundPos (Pos y x) = Coord (round y) (round x)

demos :: IO ()
demos = do
  demo8

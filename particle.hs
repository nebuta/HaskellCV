module Main where

import CV
import CV.Filter
import CV.Demo
import CV.Draw

import Data.List (sort)
import Debug.Trace (trace,traceShow)

import Control.Concurrent (forkIO)

import Text.Printf

findParticles :: GrayImage -> [Pos]
findParticles = detect . prefilter
  where prefilter = gauss (fromIntegral minDist)

minDist = 3 :: Int

detect :: GrayImage -> [Pos]
detect img = refinePos $ filterThresAndDist minInt (fromIntegral minDist) img $ findIndexMat cmpEqual img dilated
  where
    perc = 10.0   -- in %
    minInt = fromIntegral $ percentile perc img
    dilated = dilate (fromStrEl (Ellipse minDist minDist)) img

findIndexMat :: CmpFun a b -> MatT a b -> MatT a b -> [Coord]
findIndexMat f a b = findNonZero $ CV.compare f a b

refinePos :: [Coord] -> [Pos]
refinePos cs = map f cs
  where f (Coord y x) = Pos (fromIntegral y) (fromIntegral x)--Stub

filterThresAndDist :: Double -> Double -> MatT U8 C1-> [Coord] -> [Coord]
filterThresAndDist int dist mat ps = filter (\p@(Coord y x) -> f int mat p && g (map h ps) dist (p,pixelAt y x mat)) ps
  where
    f :: Double -> MatT U8 C1-> Coord -> Bool
    f int mat (Coord y x) = (fromIntegral $ pixelAt y x mat) >= int
--    f _ _ _ = True 
    g :: [(Coord,Int)] -> Double -> (Coord,Int) -> Bool
    g ps dist p@(_,int) = all (\(_, intOther) -> intOther <= int) $ filter (adjacent dist p) ps
    adjacent :: Double -> (Coord,Int) -> (Coord,Int) -> Bool
    adjacent d (c1,_) (c2,_) = let dd = distSq c1 c2 in dd > 0.1 && dd < d*d
    h :: Coord -> (Coord,Int)
    h c@(Coord y x) = (c, pixelAt y x mat)
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
filename n = "./img/cell"++printf "%03d" n++".jpg"

main :: IO ()
main = do
  first <- readImg $ filename 1
  vw <- newVideo "out.mpg" MPEG1 30 (numRows first) (numCols first)
  pss <- mapM_ (processOneFrame vw) [1..600]
  return ()

processOneFrame :: VideoWriter -> Int -> IO [Pos]
processOneFrame vw n = do 
  img <- readImg (filename n)
  putStrLn $ "Frame: " ++ show n
  let ps = findParticles (convert img)
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

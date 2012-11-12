module Main where
import Plot
import Graphics.GD
import Data.Complex
import System.Process
import Control.Monad

imageDim :: Size
imageDim = (200,200)

maxIter = 100

-- Coordinates in the complex plane to graph
-- First coordinate must be top left
mbWindow :: Window
-- mbWindow = ( (-1.8, 0.02), (-1.7, -0.08) ) -- smaller ship on horizon
mbWindow = ( (-1.9, 1.3), (0.7, -1.3) ) -- Full view

main :: IO ()
main = do
  drawPlot drawShip imageDim mbWindow "burning_ship.png"
  a <- runCommand "xv burning_ship.png"
  putStr "Fertig"

drawShip :: Coordinate -> Color
drawShip (x,y) = colorIterations $ ship (x :+ y) (0 :+ 0) 0

-- Count number of iterations for the given point to diverge
-- Non-diverging points return 0
ship :: Complex Double -- Coordinate to test
           -> Complex Double -- Iterating Z value
           -> Int -- Current iteration
           -> Int -- Iterations before diverging
ship c z iter
    | iter > maxIter = 0
    | otherwise = let z' = z**(2) + c
		  in
                  if magnitude z' > 2
                  then iter
		  else ship c z' (iter+1)

colorIterations :: Int -> Color
colorIterations x
    | x > maxIter = rgb 255 255 maxIter
    | otherwise = rgb (x*2) x x
import Graphics.UI.GLUT
import Control.Monad
import Data.Int
import Data.Complex

maxIter = 50

x // y = fromIntegral x / fromIntegral y

-- Divides [a] into [[a], [a], ...] with each sublist of length n,
-- except the last sublist which has length <= n.
chunkify n [] = []
chunkify n xs = let (xs', rest) = splitAt n xs
                in xs' : chunkify n rest

-- Converts a coordinate in screen space to a vertex.
pix2vert (Size w h) (x, y) = Vertex2 ((3 // w * fromIntegral x) - 2.0)
                             ((2 // h * fromIntegral y) - 1.0)

-- List of all of the vertices that represent screen pixels.
vertices :: IO [Vertex2 GLfloat]
vertices = get windowSize >>= \(Size w h) ->
           return $ [pix2vert (Size w h) (x, y) | x <- [0..w-1], y <- [0..h-1]]
           
drawVert v = do color . getcolor $ mandel v
                vertex v

-- draws all the vertices in slices (to update the display while drawing).
display' chunks = do mapM_ (\vs -> do renderPrimitive Points $ mapM_ drawVert vs
                                      flush) chunks
                     displayCallback $= display

-- draws the whole fractal
display = do clear [ ColorBuffer ]
             displayCallback $= (vertices >>= display' . chunkify 256)
             get currentWindow >>= postRedisplay

main = do
   getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 1280 1024
   initialWindowPosition $= Position 100 100
   createWindow "Mandelbrot"
   clearColor $= Color4 0 0 0 0
   matrixMode $= Projection
   loadIdentity
   ortho (-2) 1 (-1) 1 (-1) 1
   displayCallback $= display
   mainLoop

   
   
   
--rgbFromHsv h s v = hsvToRGB $ HSV h s v
--color3Hsv h s v = Color 
  --where
   -- rgb = rgbFromHsv h s v

-- Gets the color for a number of maxIter.
color3 r g b = Color3 r g b
getcolor :: Int -> Color3 Float
getcolor iter | iter == maxIter = color3 0 0 0
              | otherwise       = color3 (amt*0.5) (amt) (amt)
              where amt = iter // maxIter

-- Returns the number of maxIter <= the maximum iterations of the
-- Mandelbrot set at the given vertex.
mandel (Vertex2 r i) = length . takeWhile (\z -> magnitude z <= 8) .
                       take maxIter $ iterate (\z -> conjugate(sinh z)^2 + (r :+ i)) 0


                       
-- mandel (Vertex2 r i) = length . takeWhile (\z -> magnitude z <= 25) . take maxIter $ iterate (\z -> z^2 + 2*z + ((r+2*imagPart(z)) :+ (i-realPart(z)))) 0
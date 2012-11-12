import Graphics.Gnuplot.Simple
import Data.Ratio

f :: Rational -> Rational
f x
 | x >= 0 && x <= 0.5 = 2*x
 | x > 0.5 && x <= 1 = 2-2*x

f2 :: Rational -> Rational
f2 x
 | x >= 0 && x <= 0.5 = 2*x
 | x > 0.5 && x <= 1 = 2*x - 1

f3 :: (Rational,Rational) -> (Rational,Rational)
f3 (x,y)
  | x >= 0 && x <= 0.5 = (2*x,y/2)
  | x > 0.5 && x <= 1 = (2-2*x,1-y/2)

f4 :: (Rational,Rational) -> (Rational,Rational)
f4 (x,y)
  | x >= 0 && x <= 0.5 = (2*x,y/2)
  | x > 0.5 && x <= 1 = (2*x-1,(1+y)/2)

d :: Int -> [Double]
d x = map fromRational $ take x $ iterate f2 (1/123456)

--d2 :: Int -> [(Double,Double)]
--d x = map 
m x = do plotList [] (d x)


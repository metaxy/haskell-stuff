import Data.List
import GHC.Real

gen n x = (x*13 + 17)`mod` n
gen2 n = map (gen n)
sample = [1 .. 1000]

l = map (\x -> gen2 x sample) [2,3,17,32]

--entropie :: [Integer] -> Float
entropie l = sum $ map calc l
    where
        f x = (fromIntegral $ count x l) / (fromIntegral len)
        calc 0 = 0
        calc x =  (f x) * log (f x)
        len = length l

count :: (Eq a) => a -> [a] -> Int
count x = length . filter(== x)

dup x = any (> 1) $ map (\y -> count y x) x

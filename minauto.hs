-- Simplify a machine
import Data.List
import Data.Maybe
type Input = Int
type State = Int
type Table = [Cell]
type Cell = ((State,State),Bool)
-- Defintion of the machine
q :: [State]
q = [0,1,2,3,4]
sigma = [0,1]
f = [4]
d :: State -> Input -> State
d 0 0 = 1
d 0 1 = 2
d 1 0 = 4
d 1 1 = 2
d 2 0 = 3
d 2 1 = 2
d 3 0 = 4
d 3 1 = 0
d 4 _ = 4
d _ _ = 5

--The Alogrithm
tab :: Table
tab = [((i,j),((elem i f) /= (elem j f))) | i <- q, j <- q, i /= j]

mark f (a,b)
 | f a = (a,True)
 | otherwise = (a,b)

test x y = (map fst (filter (\o -> snd o == True) x) `intersect` (scanl dd y sigma)) /= []

dd (x,y) s = (d x s, d y s)

simplify = until (\x -> simplify' x == x) simplify'
simplify' x = map (mark (test x)) x

-- Output
m = map fst $ nubBy (\x y -> fst (fst x) == snd (fst y) && fst (fst y) == snd (fst x)) $ filter (\x -> snd x == False) $ simplify tab


test2 = concat $ map (\x -> map (\y -> (x,y, (nD x y))) sigma ) (q \\ (map fst m))
nD s i 
  | index == -1 = d s i
  | otherwise = snd $ m !! index
   where 
    index = fromMaybe (-1) (elemIndex (d s i) (map fst m))

main = do 
    	putStr "Q = "
	print (q \\ (map fst m))
	putStr "\nF = "
	print f
	putStr "\nSigma = "
	print sigma
	putStr "\n"
	mapM_ putD test2

putD (a,b,c) = do putStrLn $ (show a) ++ " x " ++ (show b) ++  " -> " ++ (show c)


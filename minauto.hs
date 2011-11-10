import Data.List
import Data.Maybe
type Input = Int
type State = Int
type Table = [((State,State),Bool)]

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

--usw.
tab :: Table
tab = [((i,j),False) | i <- q, j <- (delete i q)]

m =  filter  (\x -> snd x == False) $ doo $ map (mark el) tab

el (a,b)
 | a `elem` f && (not $ b `elem` f) || b `elem` f && (not $ a `elem` f) = True
 | otherwise = False

mark f (a,b)
 | f a = (a,True)
 | otherwise = (a,b)

doo x
 | new == x = x
 | otherwise = doo new
 where
  new = map (mark (test x)) x

test :: Table -> (State,State) -> Bool
test x y@(a,b) = foldl (||) False (map(\p -> fromMaybe False (lookup p x)) (map (\o -> ((d (fst (fst o)) (snd o)),(d (snd (fst o)) (snd o)))) (zip [y] sigma)))


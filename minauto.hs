import Data.List
import Data.Maybe
type Input = Int
type State = Int
type Table = [((State,State),Bool)]

q :: [State]
q = [0,1,2,3,4]
sigma =  [0,1]
f = [4]
δ :: State -> Input -> State
δ 0 0 = 1
δ 0 1 = 2
δ 1 0 = 4
δ 1 1 = 2
δ 2 0 = 3
δ 2 1 = 2
δ 3 0 = 4
δ 3 1 = 0
δ 4 _ = 4
δ _ _ = 5

--usw.
tab :: Table
tab = [((i,j),False) | i <- q, j <- (delete i q)]

m = map (mark el) tab

el (a,b) 
 | a `elem` f && (not $ b `elem` f) = True
 | b `elem` f && (not $ a `elem` f) = True
 | otherwise = False

mark f (a,b) 
 | f a = (a,True)
 | otherwise = (a,False)

doo x 
 | new == x = x
 | otherwise = doo new
 where
  new = map (mark (test x)) x
   where
    g d@((a,b),c)  
     | c == True = d
     | test x (a,b) == True = ((a,b),True)
     | otherwise = d


test :: Table -> (State,State) -> Bool

test x y@(a,b) =  foldl (||) False (map(\p -> fromMaybe False (lookup p x)) (map (\o -> ((δ (fst (fst o)) (snd o)),(δ (snd (fst o)) (snd o)))) (zip [y] sigma)))

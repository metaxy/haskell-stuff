-- http://www.haskell.org/pipermail/haskell-cafe/2005-November/012009.html only def
data BTree = BTree [Int] [BTree] deriving (Show, Eq)

t = 2 -- t ist die minimale anzahl an kindknoten
if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

isLeaf (BTree x y) = length y == 0
       
search :: Int -> BTree -> (Int, BTree)
search s t@(BTree x y) = if' (isLeaf t) (searchLeaf s t) (searchInnerNode s t)
                            
searchInnerNode s t@(BTree x y) = if' (firstPos s x 0 == -2) (search s (last y)) (if' (x !! (firstPos s x 0) == s) (((firstPos s x 0),t)) (search s (y !! firstPos s x 0)))
    where
        firstPos s (x:xs) i
            | x >= s = i
            | otherwise = firstPos s xs (i+1)
        firstPos _ [] _ = -1
        
 
searchLeaf :: Int -> BTree -> (Int, BTree)
searchLeaf s t@(BTree x y) = if' ((s' s x 0) == -1) (-1,(BTree [] [])) ((s' s x 0),t)
    where
        s' s (x:xs) i
            | x == s = i
            | otherwise = s' s xs (i+1)
        s' s [] i = -1
            
insert value (BTree [] _) = (BTree [value] [])
insert value t@(BTree x y ) = if' (fst (search value t) == -1) (insert2 value t (BTree [] [t]) 0) (t)

-- todo: it use the nT as if it is splited
-- but it does not return the p but it is also modified
insert2 :: Int -> BTree -> BTree -> Int -> BTree    
insert2 v t p i = BTree (getX nTT) (replaceChild (getY nTT) nT (if' (isLeaf t) (insertLeaf v nT) (insertInnerNode v nT p i)))
    where
        nT = if' (isFull t) (elemA nTT i) t
        elemA (BTree x y) i = y !! i
        nTT = if' (isFull t) (splitChild p i) t 
        getX (BTree x y) = x
        getY (BTree x y) = y

insertLeaf :: Int -> BTree -> BTree
insertLeaf v t@(BTree x y) = (BTree (a v x 0 []) y)
    where
        a:: Int -> [Int] -> Int -> [Int] -> [Int]
        a v (x:xs) i b 
            | v > x = a v xs (i+1) (b++[x])
            | otherwise = b ++ [v,x] ++ xs
        a v [] _ b = b ++ [v]

insertInnerNode :: Int -> BTree -> BTree -> Int -> BTree    

insertInnerNode v t@(BTree x y) p i =insert2 v child t i
    where
        firstPos v (x:xs) i
            | x > v = i
            | otherwise = firstPos v xs (i+1)
        firstPos _ [] _ = -2
        fPos = firstPos v x 0
        child = if'(fPos == -2 ) (last y) (y !! fPos)
        i = if'(fPos == -2 ) (length y) (fPos) 
               
replaceChild :: [BTree] -> BTree -> BTree -> [BTree]
replaceChild list old new = r' list old new []
    where
        r' (x:xs) old new b 
            | x == old = b ++ [new] ++ xs
            | otherwise = r' xs old new (b ++ [x])


splitChild :: BTree -> Int -> BTree
splitChild root@(BTree x y) i = split' root (y !! i)
    where
        split' (BTree x y) c@(BTree x1 y1) = (BTree (insertInRightPos x (middleValue x1)) (replace y c (BTree (take (t-1) x1) (take (t-1) y1)) (BTree (drop t x1) (drop t y1))))

middleValue :: [Int] -> Int
middleValue x = x !! (t-1)

insertInRightPos :: [Int] -> Int -> [Int]
insertInRightPos x y = i' x y []
    where
        i' (x:xs) y n
            | x < y = i' xs y (n ++ [x])
            | otherwise = n ++ [y,x] ++ xs
        i' [] y n = n ++ [y]
    
replace :: [BTree] -> BTree -> BTree -> BTree -> [BTree]
replace y c new1 new2 = replace' y c new1 new2 []
    where
        replace' (y:ys) c new1 new2 pre
            | y == c = pre ++ [new1,new2] ++ ys
            | otherwise = replace' ys c new1 new2 ( pre ++ [y])

isFull (BTree x y) = length x == 2*t-1
testTree = BTree [13,55] [(BTree [5,6] []),(BTree [23,41] []),(BTree [60,72,91] [])]
emptyTree = BTree [] []

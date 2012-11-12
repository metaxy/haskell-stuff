module BTree (
    BTree,
   search,
    insert,
    delete,
    emptyTree,
    testTree,
    insertALot,
    insertALot2,
    insertALot3,
    insertALot4
    
) where 
    data (Ord a) => BTree a = BTree [a] [BTree a] deriving (Show, Eq)
    t = 2 -- Ordnung

    -- if the given BTree is a leaf, it returns true, otherwise false.
    isLeaf :: (Ord a) => BTree a -> Bool
    isLeaf (BTree x y) = length y == 0

    -- if the given BTree is full, it returns true, otherwise false.
    isFull :: (Ord a) => BTree a -> Bool
    isFull (BTree x y) = length x == 2*t-1

    -- search after the given value in the BTree. If there is match it returns a pair
    -- consisting of the key index and the it containing node. 
    -- Otherwise it returns (-1, BTree [] []).
    search :: (Ord a) => a -> BTree a -> (Int, BTree a)
    search s tree@(BTree x y)
        | isLeaf tree = searchLeaf s tree
        | otherwise = searchInnerNode s tree
     
    searchInnerNode :: (Ord a) => a -> BTree a -> (Int, BTree a)
    searchInnerNode s tree@(BTree x y)
        | fPos == -1 = search s (last y)
        | x !! (fPos) == s = (fPos,tree)
        | otherwise = search s (y !! fPos)

        where
            firstPos :: (Ord a) => a -> [a] -> Int -> Int
            firstPos s (x:xs) i
                | x >= s = i
                | otherwise = firstPos s xs (i+1)
            firstPos _ [] _ = -1 --not found
            fPos = firstPos s x 0
            
    
    searchLeaf :: (Ord a) => a -> BTree a -> (Int, BTree a)
    searchLeaf s tree@(BTree x y) 
        | ((s' s x 0) == -1) = (-1,(BTree [] []))
        | otherwise = ((s' s x 0),tree)
            where
                s' s (x:xs) i
                    | x == s = i
                    | otherwise = s' s xs (i+1)
                s' s [] i = -1
    -- Insert a value in the BTree. It the tree already contains it, than the tree
    -- will not be modified.
    insert :: (Ord a) => a -> BTree a -> BTree a
    insert value (BTree [] _) = (BTree [value] [])
    insert value r@(BTree x y ) 
        | (fst (search value r) == -1) = insert2 value (if (isFull r) then (splitRoot r) else r) 
        | otherwise  = r                                                 
    --todo: if t > 2 move more values up
    splitRoot :: (Ord a) => BTree a -> BTree a
    splitRoot (BTree x y) = BTree 
                            newX
                            []
        where
            newX = [x !! (t-1)]
            newY = ((BTree (take (t-1) x) (take (t-1) y)),
                    (BTree (drop t x) (drop t y)))

    insert2 :: (Ord a) => a -> BTree a -> BTree a   
    insert2 insertKey tree
        | (isLeaf tree) = (insertLeaf insertKey tree)
        | otherwise = (insertInnerNode insertKey tree)

    insertLeaf :: (Ord a) => a -> BTree a -> BTree a
    insertLeaf insertKey (BTree x y) = (BTree (newKeyList insertKey x 0 []) y)
        where
            newKeyList insertKey (x:xs) i b 
                | insertKey > x = newKeyList insertKey xs (i+1) (b++[x])
                | otherwise = b ++ [insertKey,x] ++ xs
            newKeyList insertKey [] _ b = b ++ [insertKey]

    insertInnerNode :: (Ord a) => a -> BTree a -> BTree a  
    insertInnerNode v tree@(BTree x y) =  BTree 
                                        (getX newNode) 
                                        (replaceChild
                                            (getY newNode) 
                                            child 
                                            (insert2 v child)
                                        )
        where
            firstPos v (x:xs) i
                | x > v = i
                | otherwise = firstPos v xs (i+1)
            firstPos _ [] _ = -1
            oldfPos = firstPos v x 0
            fPos = firstPos v (getX newNode) 0
            newNode 
                | (isFull oldChild) = (splitChild tree oldi) 
                | otherwise = tree
            oldChild 
                | (oldfPos == -1) = (last y) 
                | otherwise = (y !! oldfPos)
            child  
                | (fPos == -1) = (last (getY newNode)) 
                | otherwise = ((getY newNode) !! fPos)
            i 
                | (fPos == -1) = (length y - 1) 
                | otherwise = (fPos) 
            oldi
                | (oldfPos == -1) = (length y - 1) 
                | otherwise = (oldfPos) 
            getX (BTree x y) = x
            getY (BTree x y) = y
                
                
    replaceChild :: (Ord a) => [BTree a] -> BTree a -> BTree a -> [BTree a]
    replaceChild list old new = r' list old new []
        where
            r' (x:xs) old new b 
                | x == old = b ++ [new] ++ xs
                | otherwise = r' xs old new (b ++ [x])


    splitChild :: (Ord a) => BTree a -> Int -> BTree a
    splitChild (BTree x y) i = split' (y !! i)
        where
            split' c@(BTree x1 y1) = BTree 
                                    (insertInRightPos x (middleValue x1)) 
                                    (replace y c 
                                    (BTree (take (t-1) x1) (take (t-1) y1)) 
                                    (BTree (drop t x1) (drop t y1))
                                    )

    middleValue :: (Ord a) => [a] -> a
    middleValue x = x !! (t-1)

    insertInRightPos :: (Ord a) => [a] -> a -> [a]
    insertInRightPos x y = i' x y []
        where
            i' (x:xs) y n
                | x < y = i' xs y (n ++ [x])
                | otherwise = n ++ [y,x] ++ xs
            i' [] y n = n ++ [y]
        
    replace :: (Ord a) => [BTree a] -> BTree a -> BTree a -> BTree a -> [BTree a]
    replace y c new1 new2 = replace' y c new1 new2 []
        where
            replace' (y:ys) c new1 new2 pre
                | y == c = pre ++ [new1,new2] ++ ys
                | otherwise = replace' ys c new1 new2 (pre ++ [y])


    getAllKeys :: (Ord a) => BTree a -> [a]        
    getAllKeys (BTree x y) = x ++ (childKeys y)
        where
            childKeys [] = []
            childKeys (y:ys) = getAllKeys y ++ childKeys ys
            
    insertList :: (Ord a) => [a] -> BTree a -> BTree a
    insertList (x:xs) t = insertList xs (insert x t)
    insertList [] t = t

    delete :: (Ord a) => a -> BTree a -> BTree a
    delete value tree@(BTree x y) 
        | (fst s == -1) = (tree) 
        | otherwise = (insertList l (BTree [] []))
        where
            s = search value tree 
            l = ldelete value (getAllKeys tree) 
            
    --some tests
    testTree = BTree 
            [13,55] 
            [(BTree [5,6] []),(BTree [23,41] []),(BTree [60,72,91] [])]
            
    emptyTree = BTree [] []

    insertALot = insert 6(insert 5(insert 4(insert 3(insert 2(insert 1 emptyTree)))))
    insertALot2 = insert 5(insert 4(insert 3(insert 2(insert 1 emptyTree))))
    insertALot3 = insert 7(insert 8(insert 4(insert 3(insert 2(insert 1 testTree)))))
    insertALot4 = insert 7(insert 4(insert 3(insert 2(insert 1 testTree))))

    ldelete                :: (Ord a) => a -> [a] -> [a]
    ldelete _ []        = []
    ldelete x (y:ys)    = if x == y then ys else y : ldelete x ys
    


module Set (
	Set,
	empty,
	insert,
	delete,
	--isEmpty,
	--isElement,
	--card,
	--intersection,
	--union,
	--diff,
	--show
) where 

	data Set a = Empty | S a (Set a) deriving(Eq,Show)

	empty :: Set a
	empty = Empty

	-- insert an element in the set wenn nicht vorhanden
	insert :: a -> Set a -> Set a
	insert x s = S x s 

	-- removes an element from the set
	--delete empty s = s
	delete _ Empty = Empty
	delete b (S s rs) = if' (s == b) (S empty (delete b rs)) (S (S s empty) (delete b rs))
	
	
	if' True x _ = x
	if' False _ y = y 
	--is the set an empty set
	--isEmpty :: Set a -> Bool

	-- is a Element in the set
	--isElement :: a -> Set a -> Bool

	-- the number of elements in the set
	--card :: Set a -> Int

	--The intersection of two set
	--intersection :: Set a -> Set a -> Set a

	--the union of two sets
	--union :: Set a -> Set a -> Set a

	--the difference of two sets
	--diff :: Set a -> Set a -> Set a

	--show :: Set a -> String

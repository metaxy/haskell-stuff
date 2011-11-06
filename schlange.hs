module Schlange (
	Schlange,
	empty,
	isEmpty,
	enqueue,
	top,
	dequeue
	) where
	type Schlange a = [a] 
	isEmpty x = null x
	empty = []
	enqueue x s = s ++ [x]
	
	dequeue [] = error "empty"
	dequeue (x:xs) = xs
	
	top [] = error "empty"
	top (x:xs) = x
	


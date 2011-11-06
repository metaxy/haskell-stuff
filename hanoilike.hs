import Data.List

data Box = A | B | C | D | X deriving (Show,Eq)

type Zeile = (Box,Box,Box,Box)

type All = (Zeile,Zeile)
start :: All
start = ((X,X,X,X),(A,B,C,D))

end :: All
end = ((X,X,X,X),(B,A,C,D))

shortes :: All -> [All] 
shortes x = x : shortes' x []
	where
		shortes'  :: All  -> [All] -> [All]
		shortes' x list 
		  | (x == end) = list
		  | otherwise = shortes' (last c) (c ++ list)
		  	where c = change x end

change :: All -> All -> [All]
change o p = swap o p

per  [] = [[]]
per xs =[x:ys | x <- xs, ys <- per (delete x xs)]

-- list of list to list of tuples
perin x = perin' x []
 where
  perin' (x:xs) s = perin' xs (s ++ [(x !! 1, x !! 2)])
  perin' [] s = s

swap :: All -> All -> [All]
swap a b = rolling a b swap2' (perin (per ["fst", "snd", "thr", "frd"]))

rolling a b f perm = roll [] [a] perm
  where
    roll list lastres [] = list
    roll list lastres (x:arg) = roll (list ++ res) (res) (arg)
        where
	     res = f (last lastres) b x

cont (x:xs) a = delete x a
cont [] a = a

allPos = ["fst", "snd", "thr", "frd"]

swap2' x y (a,b) = swap2 x y (sg a) (sg b) (sg c) (ss a) (ss b) (ss c)
  where c = last (cont [a,b] allPos)

swap2 :: All -> All -> (Zeile -> Box) -> (Zeile -> Box) -> (Zeile -> Box) -> (Box ->Zeile-> Zeile) -> (Box ->Zeile-> Zeile) -> (Box ->Zeile-> Zeile) -> [All]

swap2 (o,a) (p,b) c1 c2 c3 s1 s2 s3
 | ((c1 a) == (c2 b)) && ((c2 a) == (c1 b)) = [st1,st2,st3]
 | otherwise = [(o,a)]

 where
  st1 = ((s3 (c1 b) o), (s2 X a))
  st2 = (fst(st1)          , (s1 X (s2 (c2 b) (snd st1))))
  st3 = ((s3 X (fst st2))     , (s1 (c1 b) (snd st2)))


_fst (a,_,_,_) = a
_snd (_,a,_,_) = a
_thr (_,_,a,_) = a
_frd (_,_,_,a) = a

setfst x (a,b,c,d)  = (x,b,c,d)
setsnd x (a,b,c,d)  = (a,x,c,d)
setthr x (a,b,c,d)  = (a,b,x,d)
setfrd x (a,b,c,d)  = (a,b,c,x)

ss "fst" = setfst
ss "snd" = setsnd
ss "thr" = setthr
ss "frd" = setfrd

sg "fst" = _fst
sg "snd" = _snd
sg "thr" = _thr
sg "frd" = _frd

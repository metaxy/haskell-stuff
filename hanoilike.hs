data Box = A | B | C | D | X deriving (Show,Eq)

type Zeile = (Box,Box,Box,Box)

type All = (Zeile,Zeile)
start :: All
start = ((X,X,X,X),(A,B,C,D))

end :: All
end = ((X,X,X,X),(D,B,A,C))


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

swap :: All -> All -> [All]
swap a b = roll [] a [("fst","frd"), ("fst","snd") ("snd", "thr"),("thr", "frd")]
  where
    sw1 x = swap3 x b "fst" "frd" "snd"
    sw2 x = swap3 x b "fst" "snd" "frd"
    sw3 x = swap3 x b "snd" "thr" "frd"
    sw4 x = swap3 x b "thr" "frd" "snd"

    r1 = sw1 a
    r2 = sw2 (last r1)
    r3 = sw3 (last r2)
    r4 = sw4 (last r3)

    roll list lastres (x:arguments) = roll (list ++ res) (res) (arguments)
    roll list lastres [] = list
    	where 
	  res = swap3 (last lastres) b arguments

--  | a == z && d == w = [((X,d,X,X),(a,b,c,X)),((X,d,X,X),(X,b,c,a)),((X,X,X,X),(d,b,c,a))]

-- c1 = fst
-- c2 = frd
cont "fst" _ = "snd"
cont "fst" "snd" = "thr"
cont "snd" _ = "fst"
cont "snd" "fst" = "thr"
cont "thr" _ = "fst"
cont "thr" "fst" = "snd"
cont "frd" _ = "snd"
cont "frd" "snd" = "thr"

swap3 x y (a,b) = swap2 x y (sg a) (sg b) (sg c) (ss a) (ss b) (ss c)
  where c = cont a b

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

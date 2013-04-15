import Data.List

type Var = (Integer,Bool)
type Minterm = [Var]
type DNF = [Minterm] -- disjunktive Normalform
type KDNF = [Minterm] -- kanonisch diskunjtive Normalform

-- gegeben nach step 1
f :: DNF
f = [
        [(1,True),(2,True),(4,True)],
        [(1,True),(3,True)],
        [(2,True),(3,False)],
        [(3,False),(4,False)]
    ]
-- notwendig nach step 1
grad :: DNF -> Integer
grad = lmax . map (lmax) . map(map(fst))
lmax = foldl max 0

atLeastOne = foldl (||) False

search' :: Var -> Minterm -> Bool
search' s x =  (<) 0 $ length $ filter(\y -> y == s) x

search :: Var -> [Minterm] -> Bool
search s = atLeastOne . map (search' s)

-- schaut ob zwei terme sich überlappen
overlap :: Minterm -> Minterm -> Bool
overlap x y = equal a && equal b
    where
        a = zipping fst x y -- combine numbers
        b = zipping snd x y -- combine negated or not
        zipping f a b = unzip $ zip (f $ unzip a) (f $ unzip b)

equal x = fst x == snd x

deleteOverlap' :: Minterm -> [Minterm] -> [Minterm]
deleteOverlap' c x = filter (not . overlap c) x

deleteOverlap :: DNF -> DNF
deleteOverlap [] = []
deleteOverlap (x:xs) = x:(deleteOverlap' x (deleteOverlap xs))

-- bekommt ein DNF rein und schaut nach welche variable (sowohl negiert als auch nicht) verkommen
-- notwendig für step 1
both :: [Minterm] -> [Integer]
both x = filter (\y -> (search (y,True) x) && (search (y,False) x)) $ [0..(grad x)]

primimpl' :: DNF -> [(Minterm,Minterm)]
primimpl' x = flatten1 $ map (\(o,p) -> combi o p) $ zip j n
        where
        jo number = filter (\y -> search'(number,True) y) x
        no number = filter (\y -> search'(number,False) y) x
        j = map jo $ both x
        n = map no $ both x

deleteBoth :: Minterm -> Minterm
deleteBoth x = filter (\(a,b) -> not $ (search' (a,(not b)) x)) x

primimpl :: DNF -> DNF
primimpl = map deleteBoth . map (\(x,y) -> x ++ y) . primimpl'

f1 = [(1,True),(2,True),(4,True)]
f2 = [(1,True),(2,True)]
f3 = [(1,True)]
f4 = [(3,False),(4,False)]

x = ["x1","x2"]
y = ["y1", "y2", "y3"]
-- tools:
combi x y = map head $ map(zip x) (permutations y)

flatten1 = foldl (++) []

asd = [(1,True),(2,True),(4,True),(2,False),(3,False)]


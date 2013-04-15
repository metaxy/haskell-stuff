import Data.List
import Data.Maybe

type Var = (Int, Bool)
type Minterm = [Var]
type Tabelle = [Zelle]
type Zelle = (Minterm,[Int])
type Table a b = [[(a,b)]]
data In a = W | F | N
instance Show a => Show (In a) where
        show W = "W"
        show F = "F"
        show N = "-"

-- run all on input
m = col (dominanz $ primtermtable $ primes $ (tabelle input)) 0

input :: [Minterm]
{--input = create (		
		[[F,F,F,F,W],
		[F,F,F,W,W],
		[F,F,W,F,W],
		[F,F,W,W,W],
		[F,W,F,F,W],
		[F,W,F,W,W],
		[F,W,W,F,W],
		[F,W,W,W,W],
		[W,F,F,F,W],
		[W,F,F,W,W],
		[W,F,W,F,W],
		[W,F,W,W,W],
		[W,W,F,F,W],
		[W,W,F,W,W],
		[W,W,W,F,W],
		[W,W,W,W,W],

		[F,F,W,W,F],
		[F,W,F,W,F],
		[F,W,W,W,F],
		[W,F,F,W,F],
		[W,F,W,W,F],
		[W,W,F,W,F],
		[W,W,W,F,F],
		[W,W,W,W,F]])
--}
input = create( [
    [F,F,W],
    [F,W,F],
    [F,W,W],
    [W,F,W],
    [W,W,F]])

-- klasse eines mintermes
cl :: Minterm -> Int
cl x = length (filter ((== True).snd) x)

primes :: Tabelle -> Tabelle
primes = nub . primes' []
primes' p before
    | next == [([],[])] || next == [] = (p ++ before )
    | otherwise = primes' (p ++ left) next
    where
        next = nub (fst u)
        left = nub (snd u)
        u = umformen before before [] []


umformen :: Tabelle -> Tabelle -> Tabelle -> Tabelle -> (Tabelle,Tabelle)
umformen all (x:xs) newPairs used
    | found == True = umformen all xs (newPairs ++ new) (used ++ u)
    | found == False = umformen all xs newPairs used
    where
        y' = findSomeone all x [] []
        u = fst y'
        new = snd y'
        found = not(new == [])

umformen all [] newPairs used = (newPairs, all \\ used)

findSomeone all x = findSomeone' (filter (clOK x) all) x

findSomeone' (y:ys) x used new
    | (differ (fst x) (fst y)) == 1 && onSame (fst x) (fst y) = findSomeone' ys x (used ++ [x] ++ [y]) (new ++ [(n x y)])
    | otherwise = findSomeone' ys x used new
    where
            n x y = ((zipWith rm (fst x) (fst y)), sort((snd x) ++ (snd y) ++ (add x) ++ (add y)))
findSomeone' [] x used new = (used, new)

clOK x y = abs (cl(fst x) - cl(fst y)) == 1

add a
    | pos (fst a) /= -1 = [pos (fst a)]
    | otherwise = []

differ :: Minterm -> Minterm -> Int
differ x y = length (filter (== True) (zipWith (\a b -> (fst a == fst b && snd a /= snd b)) x y))

onSame :: Minterm -> Minterm -> Bool
onSame x y = (length (filter (== True) (zipWith (\a b -> xor (fst a == -1) (fst b == -1) ) x y))) == 0

rm :: Var -> Var -> Var
rm x y
 | (snd x /= snd y) = (-1,False)
 | otherwise = x

primtermtable :: Tabelle -> (Table Zelle Int)
primtermtable x = [[ (j,i) | i <- [0..((length input)-1)]] | j <- x]

-- can be used on a primtermtable
-- but it is used in dominanzC
toDots :: Table Zelle Int -> [[Bool]]
toDots = map toDots'

-- used by toDots'
toDots' :: [(Zelle,Int)] -> [Bool]
toDots' = map (\x -> elem (snd x) (snd (fst x)))


dominanz :: Table Zelle Int -> Table Zelle Int
dominanz x
    | (dom == x) = dom
    | ((nub dom) == [[]]) = x
    | otherwise = dominanz dom
    where
        dom = dominanzR ( dominanzC x )

dominanzC x = dominanzC' x (colCount x - 1) x
dominanzC' x (-1) all = all
dominanzC' x i all
    | dominated == True = dominanzC' x (i-1) (rmCol all pos)
    | otherwise = dominanzC' x (i-1) all
    where
        c = toDots' (col x pos)
        dots = toDots x
        dominated = (domiC c (toDots (rmCol all pos)))
        pos = i

domiC :: [Bool] -> [[Bool]] -> Bool
domiC i a = foldl (||) False (domiC' i a)

domiC' :: [Bool] -> [[Bool]] -> [Bool]
domiC' i a = map c con
    where
        con = map (zip i) (cols a)
        c x = foldl (&&) True (map (\a -> not (fst a == False && snd a == True)) x) 

dominanzR x = dominanzR' x (length x - 1) x
dominanzR' x (-1) all = all
dominanzR' x i all
    | dominated == True = dominanzR' x (i-1) (rmRow all pos)
    | otherwise = dominanzR' x (i-1) all
    where
        c = toDots' (row x pos)
        dots = toDots x
        dominated = (domiR c (toDots (rmRow all pos)))
        pos = i

domiR :: [Bool] -> [[Bool]] -> Bool
domiR i a = foldl (||) False (domiR' i a)

domiR' :: [Bool] -> [[Bool]] -> [Bool]
domiR' i a = map c con
    where
        con = map (zip i) a
        c x = (foldl (&&) True (map (\a -> not (fst a == True && snd a == False)) x))
        
-- stuff
toBool W = True
toBool F = False

create = map create'
create' x = zip [0..((length x) -1)] (map toBool x)

toIn True = W
toIn False = F

varToIn (-1) False = N
varToIn _ False = F
varToIn _ True = W

uncreate' x = map (toIn.snd) x
uncreate = map uncreate'

row :: Table a b -> Int -> [(a,b)]
row x i = x !! i

col :: Table a b -> Int -> [(a,b)]
col x i = map (!! i) x

cols x = reverse (cols' x [])
cols' x y
    | ((nub x) == [[]]) = y
    | otherwise = cols' (map (drop 1) x) ((map (\a -> (take 1 a)!!0) x):y)


testTable = [[1,2,3],[4,5,6],[7,8,9],[10,11,12]]
colCount x = length (x !! 0)

rmRow x i = delete (row x i) x
rmCol x i = map (\a -> delete (a !! i) a) x

cleanTable x = uncreate (fst (unzip (map (\a -> fst a) (col x 0))))
cleanTabelle x = map (reverse .(map (\a -> varToIn (fst a) (snd a)))) (map fst x)

pos x = fromMaybe (-1) (elemIndex x input)

pos2 x dat = fromMaybe (0) (elemIndex x dat)

tabelle :: [Minterm] -> Tabelle
tabelle x = map (\a -> (a,[])) x

xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a

boolListToInt :: [Bool] -> Int
boolListToInt x = foldl (+) 0 (map (\a -> (fst a)*(snd a)) (zip (map boolToInt (reverse x)) (iterate (*2) 1)))

minTermToBoolList :: Minterm -> [Bool]
minTermToBoolList = map (\a -> snd a)

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

showDots' :: [Bool] -> [Char]
showDots' = foldl (++) "" . (map (\a -> "| " ++ (d a) ++ " "))

showDots'' :: [[Bool]] -> [[Char]]
showDots'' = map showDots'

-- used by showDots'
d True = "*"
d False = " "


test2 = tabelle input
test3 = primes test2
test4 = primtermtable test3
test5 = dominanz test4
umformen' x = fst (umformen x x [] [])
test6 = umformen' (umformen' test2)
test7 = umformen' test2

{-input = create ([
                [F,F,F,F],
                [W,F,F,F],
                [F,F,W,F],
                [W,F,W,F],
                [F,W,W,F],
                [W,W,W,F],
                [F,F,F,W],
                [W,F,F,W],
                [W,W,F,W],
                [W,W,W,W]

                    ])-}
                    

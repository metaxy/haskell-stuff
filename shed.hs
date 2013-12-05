import Data.List
import Data.Ord
data Process = Process {
    name :: Char,
    start :: Int,
    run :: Int,
    lev :: Int
} deriving (Show)

instance Eq Process where
    (==) x y = (name x) == (name y)

data State = State {
    time :: Int
    ,active :: Process
    ,queue :: [Process]
    ,queues :: [[Process]]
    ,counter :: Int
} deriving (Show,Eq)

emptyState :: State
emptyState = State 0 (timeDown $ head example) [] [[]] 0

example :: [Process]
example = [
    (Process 'A' 0 8 0) 
    ,(Process 'B' 1 2 0)
    ,(Process 'C' 3 7 0)
    ,(Process 'D' 7 13 0)
    ,(Process 'E' 9 2 0)]

-- used in mlf
timeStep :: Int -> Int
timeStep i = 2 ^ i

runAsMuchYouWant s
    | (run $ active s) <= 0 = resetCounter $ nextProzessInQueue s
    | otherwise = incCounter s

preemp s
    | null n = runAsMuchYouWant s
    | otherwise = setActive (head n) $ appendProcess (act s) s
    where
        n = nextProzess $ time s
preemp' s
    | null n = runAsMuchYouWant s
    | otherwise = setActive (head n) $ prependProcess (act s) s
    where
        n = nextProzess $ time s


rr' slice s 
    | (counter s) == (slice-1) = resetCounter $ nextProzessInQueue $ appendProcess (act s) s
    | otherwise = runAsMuchYouWant s

fcfs s = stateTimeStep 
    $ appendProcess (nextProzess $ time s) 
    $ runAsMuchYouWant s

lcfs s = stateTimeStep 
    $ prependProcess (nextProzess $ time s) 
    $ runAsMuchYouWant s

lcfs_pr s = stateTimeStep $ preemp' s

lcfs_pr_sort s = stateTimeStep $ sortQueue(comparing run) $ preemp s

rr slice s = stateTimeStep $ rr' slice $ appendProcess (nextProzess $ time s) s


-- sortest job means shortes run time left
srtn s = stateTimeStep 
    $ sortQueue(comparing run) 
    $ appendProcess (nextProzess $ time s) 
    $ runAsMuchYouWant s

sjn s = stateTimeStep 
    $ sortQueue(comparing (run))
    $ appendProcess (nextProzess $ time s) 
    $ runAsMuchYouWant s


--multilevel feedback
mlf max s = stateTimeStep 
 	$ mlf' max 0    
	$ addToNQueue (nextProzess $ time s) 0 
        $ s

mlf' max level s 
	| (counter s) == (timeStep level) = resetCounter $ nextMlf max level s --time is up => nächsten finden
	| (run $ active s) <= 0 = resetCounter $ nextMlf max level s -- fertig! => nächsten finden
    	| otherwise = incCounter s -- derzeitigen einfach laufen lassen

nextMlf max level s
    | max == level = incCounter $ s -- alle queues leer => derzeitigen laufen lassen
    | (length $ queues s) <= level = nextMlf max level $ s{queues = (queues s) ++ [[]]}
	| null queue = nextMlf max (level + 1) s -- diese queue leer => nächste prüfen
 	| otherwise = n $ addToRightQueue (act s) s -- den ersten aus dieser queue laufen lassen
	        where
                queue = head $ drop level $ queues s
                n s = removeHeadFromQueues level $ setActive (setLevel level $ (head queue)) s

-- add a process to the right queue
addToRightQueue [] s = s
addToRightQueue [x] s = addToNQueue [x] ((lev x) + 1) s

-- add a process to the nth queue
addToNQueue p level s = s{queues = (addToNQueue' p level $ queues s)}
addToNQueue' p = modN (\x -> x ++ p)

--remove head process to the nth queue
removeHeadFromQueues level s = s{queues = modN (drop 1) level $ queues s}

modN f level q 
    | level == length q = q ++ [f []]
    | level < (length q) = start ++ [middle] ++ end
    | otherwise = error "to much"
    where
        start = take level q
        middle' = q !! level
        end = drop (level+1) q
        middle = f middle'


-- default time counter
stateTimeStep s = s{time = ((time s) + 1), 
		active = timeDown $ active s}

-- === modify state =====
-- make next process in queue active
nextProzessInQueue s = s{
    active = head $ queue s,
    queue = tail $ queue s}

-- add a process to end of queue
appendProcess p s = s{queue = (queue s) ++ p}
--add a process to begin of queue
prependProcess p s = s{queue = p ++ (queue s)}

incCounter s = s{counter = (counter s) + 1}

resetCounter s = s{counter = 0 }

sortQueue f s = s{queue = (sortBy f (queue s))}

setActive p s = s{active = p}

-- === modify process ===
-- time counter for a process
timeDown :: Process -> Process
timeDown p = p{run = (run p) - 1}

setLevel level p = p{lev = level}

-- which process to activate
nextProzess time = filter (\x -> start x == (time+1)) example

--simulate a given algo on example
sim algo = do
    let list = simulate algo
    showStateTable list

sim2 algo = do
    let list = simulate algo
    mapM_ showState list

-- run maxTime times
simulate :: (State -> State) -> [State]
simulate i = take maxTime $ iterate i emptyState


-- statitics
waitTime states = map (\x -> length $ filter (\y -> (elem x (queue y)) ||  (any (elem x) (queues y))) states) example
respTime states = map (\(x,y) -> x+y) $ zip (map run example) (waitTime states)
mean x = (fromIntegral $ sum x) / (fromIntegral $ length x)

-- helper for simulation
maxTime = sum $ (map run example)

act s
    | (run $ active s) == 0 = []
    | otherwise = [active s]


--- output helpers
showTable x = do
    let heads = map (head) x
    mapM_ (putStr . ((++) "|")) heads
    putStr "\n"
    showTable (map tail x)
    return()

-- helper to show
showState :: State -> IO ()
showState s = do
    putStr "-------" 
    putStr $ show $ time s
    putStrLn "---------"
    putStr "running: "
    print $ active s
    putStr "counter: "
    print $ counter s
    putStr "queue: "
    mapM_ putStr (map (show . name) (queue s))
    putStrLn ""
    putStr "queues: "
    print (queues s)
    putStrLn ""
    return ()

showStateTable state = do
    let list = [["___"] ++ (map (show . name) example) ++ ["cnt"]] ++ (map row state)
    --let list = (map row state)
    showTable list


row s = [show' (time s)] ++ (map (check (active s)) example) ++ [show' (counter s)] 

show' time
    | time < 10 = show time ++ " "
    | otherwise = show time

check a b
    | a == b = "❤❤"
    | otherwise = "  "


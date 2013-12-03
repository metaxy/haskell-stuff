import Data.List
import Data.Ord
data Process = Process {
    name :: Char,
    start :: Int,
    run :: Int 
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
    (Process 'A' 0 8) 
    ,(Process 'B' 1 2)
    ,(Process 'C' 3 7)
    ,(Process 'D' 7 13)
    ,(Process 'E' 9 2)]

maxTime = sum $ (map run example)

runAsMuchYouWant s
    | (run $ active s) <= 0 = resetCounter $ nextProzessInQueue s
    | otherwise = incCounter $ s

act s
    | (run $ active s) == 0 = []
    | otherwise = [active s]


preemp s
    | null n = runAsMuchYouWant s
    | otherwise = (\x -> x{active = head $ n}) $ appendProcess (act s) s
    where
        n = nextProzess $ time s

rr' slice s 
    | (counter s) == (slice-1) = resetCounter $ nextProzessInQueue $ appendProcess (act s) s
    | otherwise = runAsMuchYouWant s

fcfs s = stateTimeStep $ appendProcess (nextProzess $ time s) $ runAsMuchYouWant s
lcfs s = stateTimeStep $ prependProcess (nextProzess $ time s)$ runAsMuchYouWant s

lcfs_pr s = stateTimeStep $ preemp s

lcfs_pr_sort s = stateTimeStep $ sortQueue(comparing run) $ preemp s

rr slice s = stateTimeStep $ appendProcess (nextProzess $ time s) $ rr' slice s 
-- sortest job means shortes run time left
sjn s = stateTimeStep $ sortQueue(comparing run) $ appendProcess (nextProzess $ time s) $ runAsMuchYouWant $ s

mlf s = stateTimeStep $ addToFirstQueue (nextProzess $ time s) $ mlf' 0 s

mlf' level s 
	| (counter s) == (timeStep level) = nextMlf level s
	| otherwise = runAsMuchYouWant s

nextMlf level s
	| null $ queue = --fifo
 	| otherwise = --add active to next queue if not empy
			removeHeadFromQueues level $ s{active = (head queue)} $ 
		where
			queue = drop level $ queues s

addToFirstQueue p s = s{queues = (addToFirstQueue' p $ queues s)}

addToFirstQueue' ::  [Process] -> [[Process]] ->  [[Process]]
addToFirstQueue' p x =  ((head x) ++ p):(tail x)

removeHeadFromQueues level s = s

timeStep i = 2 ** i


-- default time counter
stateTimeStep s = s{time = ((time s) + 1), 
		active = timeDown (active s)}
-- make next process in queue active
nextProzessInQueue s = s{active= (head q),queue=(tail q)}
    where
        q = queue s

-- add a process to end of queue
appendProcess p s = s{queue = (queue s) ++ p}
--add a process to begin of queue
prependProcess p s = s{queue = p ++ (queue s)}

incCounter s = s{counter=(counter s) + 1}

resetCounter s = s{counter=0}

sortQueue f s = s{queue = (sortBy f (queue s))}
-- time counter for a process
timeDown :: Process -> Process
timeDown (Process a b time) = Process a b (time-1)

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


waitTime states = map (\x -> length $ filter (\y -> elem x (queue y)) states) example
respTime states = map (\(x,y) -> x+y) $ zip (map run example) (waitTime states)

mean x = (fromIntegral $ sum x) / (fromIntegral $ length x)

--showTable :: [[String]] -> IO ()
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
    putStr "queue:"
    mapM_ putStr (map (show . name) (queue s))
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

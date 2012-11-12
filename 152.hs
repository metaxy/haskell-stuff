import Data.Ratio
import Data.Int

type Result = [[Integer]]
type Rep = [Integer]
type IntT = Integer
val = 1 % 2
stop = 46
m = search [] 1 

search :: Rep -> IntT -> Result
search rep next | (toomuch || disttoomuch || willnever || too) = []
    where
        toomuch = next == (stop+1)
        disttoomuch = False --next > 13 && (dist r val) > (1 % ((next-1)*(next-1)))
        willnever = r + (calc [next..stop]) < val
        too = r > val
        r = calc rep

search rep _ | (calc rep == val) = [[-1]]
search rep next = new1 ++ new2
    where
        new1 = search (rep ++ [next]) (next + 1)
        new2 = search rep (next + 1)
        
calc :: Rep -> Ratio IntT
calc = foldr (+) (0 % 1) . map (\x -> 1 % (x*x))

dist a b = abs(a-b)

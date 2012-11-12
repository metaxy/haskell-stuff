import Data.List

list1 = [1,2,3,4,5,6,-1,-2,-3,-4,-5,-6]

l = map (take 5) (permutations list1)

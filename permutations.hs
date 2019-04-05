module Permutations where

perm :: [a] -> [[a]]
perm [] = [[]]
perm list = concatMap (\(a,b) -> map (a :) (perm (remove list b))) (zip list [0..])
        
remove :: (Num t, Eq t) => [a] -> t -> [a]
remove [] _ = []
remove (_:xs) 0 = xs
remove (x:xs) i = x : remove xs (i - 1)
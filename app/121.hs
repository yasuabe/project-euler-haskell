import Data.List
g n s@(x:xs) t m
    | n == 1    = t + m * (sum s)
    | length s < n   = t
    | otherwise = let t' = g (n-1) xs t (x*m) in g n xs t' m

-- this works too.
f n []       = 0
f 1 s        = sum s
f n s@(x:xs) = sum $ map g $tails s where
    g []     = 0
    g s@(x:xs) 
       | length s < n = 0
       | otherwise    = x * (f (n-1) xs)

main = print $ product [2..16] `div` sum(1:map (\n->g n [1..15] 0 1) [1..7])

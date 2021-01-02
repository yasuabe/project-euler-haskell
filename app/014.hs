import Data.Map as M

g n
    | n == 1    = 0
    | even n    = 1 + (g (n `div` 2))
    | otherwise = 1 + (g (n * 3 + 1))

f2 c map 
  | M.member c map = map 
  | otherwise      = let map' = (f2 next map) in insert c (1+map'!next) map'
  where next = if even c then div c 2 else 3*c+1
h 1 map = map
h n map
  | M.member n map = map
  | otherwise      = let map' = f2 n map in h (n-1) map'

i []         = (1,0)
i ((k,v):xs) = let (k', v') = i xs in if v < v' then (k', v') else (k, v)
     
h2 1 r        = r
h2 n r@(_, len') =
    let len = g n
        r' = if (len < len') then r else (n, len)
    in h2 (n-1) r'
  where
      g n
        | n == 1    = 0
        | even n    = 1 + (g (n `div` 2))
        | otherwise = 1 + (g (n * 3 + 1))
--837799
main = print $ fst $ h2 1000000 (0, 0)

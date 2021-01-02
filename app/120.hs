import Data.List
e120 n = n * (f n) where
    f n= if even n then n-2 else n-1
main = print $ foldl' (\a b->a + (e120 b)) 0 [3..1000]


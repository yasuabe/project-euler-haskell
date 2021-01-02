divMod' r m = if r' == 0 then (q-1, m) else t
            where t@(q, r') = divMod r m

removeAt n ds = (ht, hs ++ ts) where (hs, ht:ts) = splitAt n ds

f _ _ (h:[]) ans = 10*ans + h
f r s ds     ans = f r' (div s len) ds' (10*ans + d)
                 where (q, r')  = divMod' r s
                       (d, ds') = removeAt q ds
                       len      = length ds - 1

main = print $ f 1000000 (product [1..9]) [0..9] 0 

--ghci> (1000000 `mod` product [1..9], 1000000 `div` product [1..9])
--(274240,2)
--ghci> (274240 `mod` product [1..8], 274240 `div` product [1..8])
--(32320,6)
--ghci> (32320 `mod` product [1..7], 32320 `div` product [1..7])
--(2080,6)
--ghci> (2080 `mod` product [1..6], 2080 `div` product [1..6])
--(640,2)
--ghci> (640 `mod` product [1..5], 640 `div` product [1..5])
--(40,5)
--ghci> (40 `mod` product [1..4], 40 `div` product [1..4])
--(16,1)
--ghci> (16 `mod` product [1..3], 16 `div` product [1..3])
--(4,2)
--ghci> (4 `mod` product [1..2], 4 `div` product [1..2])
--(0,2) -> (2, 1)
--ghci> (2 `mod` product [1], 2 `div` product [1])
--(0,2) -> (1, 1)
--ghci> [0,1,2,3,4,5,6,7,8,9]!!2
--2
--ghci> [0,1,3,4,5,6,7,8,9]!!6
--7
--ghci> [0,1,3,4,5,6,8,9]!!6
--8
--ghci> [0,1,3,4,5,6,9]!!2
--3
--ghci> [0,1,4,5,6,9]!!5
--9
--ghci> [0,1,4,5,6]!!1
--1
--ghci> [0,4,5,6]!!2
--5
--ghci> [0,4,6]!!1
--4
--ghci> [0,6]!!1
--6

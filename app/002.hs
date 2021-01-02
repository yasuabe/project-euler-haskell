pe2 :: (Int, Int) -> Int
pe2 (a1, a2)
    | 4000000 < a2 = a1
    | otherwise    = (if (even a1) then a1 else 0) + pe2 (a2, a1 + a2)
main = print $ pe2 (1, 1)
-- *Main> main
-- 4613732

f n = f' n where 
    f' 0 = "0"
    f' 1 = "1"
    f' n = (if 1 == mod n 2 then '1' else '0'): f' (div n 2)

g :: Int->[Int]
g w
    | 0==mod w 2 = let hw = div w 2; in [read$(show n)++(reverse$show n)|n<-[(10^(hw-1))..(10^hw-1)]]
    | otherwise  = let hw = div w 2; in [read$(show n)++(tail$reverse$show n)|n<-[(10^hw)..(10^(hw+1)-1)]]
h' w = filter (\n->(f n)==(reverse$f n)) $ g w
h w = map (\n->(n, f n))$ g w

main = print $ sum $ concatMap h' [1..6]


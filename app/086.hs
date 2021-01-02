isSquare n = (round.sqrt$fromIntegral n)^2 ==n
isPyth a b = isSquare (a^2+b^2)

g2 a | 0 /= mod a 3 && 0/= mod a 4 = g2' 12
     | 0 /= mod a 3                = g2' 3
     | 0 /= mod a 4                = g2' 4
     | otherwise                   = g2' 1
    where g1 a b = (min a (b-1)) - (div (b+1) 2 -1)       
          g2' d=sum$map (g1 a)$filter (isPyth a) [d,d+d..2*a] 

g3 a = (a, sum$g2 a)

e86 ((a,cnt):t) acc lmt | lmt < cnt+acc = a
                        | otherwise     = e86 t (cnt+acc) lmt

main = print$e86 (map (\n->(n, g2 n)) [1..]) 0 1000000

f4 lmt a1 a2 b1 b2 s
  | lmt    <  a2   = (a1-1,  0,    s-b2)
  | (c+a0) <= lmt  = (c+a0,  d+1,  e+b2+d*a0)
  | otherwise      = (c,     d,    e)
  where a0         = a2 - a1
        b3         = ((2*a1-a2)+b1+b2)
        (c, d, e)  = f4 lmt a2 (a1+a2) b2 b3 (s+b3)

main = print ans where (_, _, ans) = f4 (10^17-1) 2 3 1 1 2

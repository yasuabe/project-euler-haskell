d0 = 3 - 2 * sqrt 3
s0 = (1/d0)^2
d d1 d2 d3 = d1 + d2 + d3 + 2 * (sqrt (d1*d2 + d2*d3 + d3*d1))

s 0 d1 d2 d3 = 0
s n d1 d2 d3 = ((1/d4)^2) + (s' d1 d2) + (s' d2 d3) + (s' d3 d1)
    where d4 = d d1 d2 d3
          s' = s (n-1) d4

main = print $ (s0 - 3 - ((s 10 1 1 1) + 3*(s 10 d0 1 1))) / s0

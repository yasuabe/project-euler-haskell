pe010 n = 2 + f 2 [3,5..n]
    where 
        f x [] = 0
        f x ys = let (ps, qs) = span (< x^2) ys
                 in  (sum ps) + (f (last ps) [z | z <- qs, all (\y->mod z y /= 0) ps])
main = print $ pe010 2000000

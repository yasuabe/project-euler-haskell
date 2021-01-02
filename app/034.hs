--brute force : limit 2999999
f n | n < 10    = fac n
    | otherwise = fac (mod n 10) + f (div n 10)
    where fac n = product [1..n]
main = print
     $ sum $ filter (\n->n==f n) [3..2999999]
--40730

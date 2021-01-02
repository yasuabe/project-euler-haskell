--ghci> find (\(n,r)->odd n && (10^10)<r)$map (\(n,a)->(n,2*a*n))$zip [1..] primes
--Just (21035,10001595590)


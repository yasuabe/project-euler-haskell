e187 ps@(h:t) | null ps'  = 0
              | otherwise = (length ps') + (e187$tail ps')
    where ps' = takeWhile (<=div (10^8) h) ps
--ghci> e187 primes

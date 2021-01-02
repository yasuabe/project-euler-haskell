e115 = (+1) $ length $ takeWhile(<10^6) $ zipWith (+) a b
    where b = 1:zipWith (+) a b
          a = 0:zipWith (+) a (replicate 48 0++[1]++b)
--ghci> e115

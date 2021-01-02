import Data.Numbers.Primes

candidates = filter (not.isPrime) $ scanl (+) 3 $ cycle [4,2,2,2]

funcA n = f 1 1
  where f m k | rem == 0  = k
              | otherwise = f (rem*10+1) (k+1)
               where rem = m `mod` n

solve = filter (\x->(x-1) `mod` funcA x == 0) $ candidates

main = print $ sum $ take 25 $ solve

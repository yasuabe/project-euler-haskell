import Data.Numbers.Primes

toNumber::[Int]->Int
toNumber = foldl (\z d -> z*10 + d) 0

merge::[Int]->[Int]->[[Int]]
merge [] ds = [ds]
merge rs [] = [rs]
merge rs@(r:rs') ds@(d:ds') =
  map (r:) (merge rs' ds) ++ map (d:) (merge rs ds')

combinations :: Int->[Int]->[[Int]]
combinations n cand = f5 n cand []
  where
    f5::Int->[Int]->[Int]->[[Int]]
    f5 0 _      rs = [rs]
    f5 _ []     _  = []
    f5 n (c:cs) rs = f5 (n-1) cand (c:rs) ++ f5 n cs rs

selectPrimes::Int->Int->Int->[Int]
selectPrimes n m d = filter isPrime
         $ map toNumber
         $ filter ((/=0).head) 
         $ concatMap (merge (take m $ repeat d)) 
         $ combinations (n-m) $ filter (/=d) [0..9]

sumPrimes::Int->Int->Int
sumPrimes n d = sum $ findPrimeSelection n (n-1) d
  where
   findPrimeSelection::Int->Int->Int->[Int]
   findPrimeSelection n m d 
    | null primeList = findPrimeSelection n (m-1) d
    | otherwise      = primeList
    where
      primeList = selectPrimes n m d

main::IO()
main = print $ sum $ map (sumPrimes 10) [0..9]

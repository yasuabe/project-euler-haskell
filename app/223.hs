import Data.Numbers.Primes

pyt1 (a,b,c) = ((-1)*a+(-2)*b+2*c,(-2)*a+(-1)*b+2*c,(-2)*a+(-2)*b+3*c)
pyt2 (a,b,c)
  | a==b      = map pyt1 [(-a,b,c), (-a,-b,c)]
  | otherwise = map pyt1 [(-a,b,c), (-a,-b,c), (a,-b,c)]
pyt3 ts
  | null l    = ts
  | otherwise = ts ++ pyt3 l
  where l = filter (\(a,b,c)->a+b+c<=25000000) $ concatMap pyt2 ts

main = print $ length $ pyt3 [(1,1,1),(1,2,2)]

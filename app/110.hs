import Data.List
import Data.Function
import Data.Numbers.Primes

f xp []       = [[1]]
f xp (x:xs)
  | xp == x   = ys
  | otherwise = (x+1:xs):ys
  where ys = [ x:y | y <- f x xs ]
g xs = ys ++ g ys
  where ys = nub $ concatMap (f 0) xs
h1 xs = (\n->div (n+1) 2)$ product $ map (\n -> 2*n + 1) xs
h2 xs = product $ zipWith (\a e->a^e) primes xs

pe110 n
  = head $ sortBy (\a b->compare a b)
  $ map h2
  $ take n $ filter (\xs-> 4000000<h1 xs) 
  $ g [[1]]
main = print $ pe110 100

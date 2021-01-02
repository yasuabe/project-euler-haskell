import Data.Numbers.Primes
import Data.List

pe027 = (\(a,b,_)->a*b)
    $ maximumBy (\(_,_,c1) (_,_,c2)->compare c1 c2) 
    $ [(a,b,f a b)|a<-[(-999)..999], b<-takeWhile (<1000)$primes, 40<f a b]
  where
    f a b = length$ takeWhile isPrime $ map (\n->n^2+a*n+b) $ [0..]

main = print $ pe027
-- -59231

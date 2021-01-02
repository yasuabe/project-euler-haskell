import Data.Maybe
import Data.List
import Data.Numbers.Primes

inverse a m = mod (f m 0 0 a 1 0) m where
    f 1  a  _  _  _  _  = a
    f r1 a1 q1 r2 a2 q2 = f r a q r1 a1 q1
      where r = mod r2 r1
            q = div r2 r1
            a = -q*a1 + a2

calcS p1 p2 m = mod (p1*p2*(inverse p2 m)) (p2*m) 

prms = dropWhile (<5) primes

e134' m = sum $ map (\(p1,p2)-> calcS p1 p2 m) 
        $ takeWhile ((<m).fst) $ dropWhile ((<div m 10).fst)
        $ zip prms (tail prms)

main = print $ sum $ map e134' [10^p|p<-[1..6]]

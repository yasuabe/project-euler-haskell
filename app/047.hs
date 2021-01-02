import Data.List
import Data.Numbers.Primes

f ns@(nh:nt)
    | all hasFourPfs $take 4 ns = nh
    | otherwise                 = f nt 
    where hasFourPfs n = 4==(length$nub$primeFactors n)

main = print $ f [1..]

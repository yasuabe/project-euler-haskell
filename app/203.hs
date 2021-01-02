import Data.Numbers.Primes
import Data.List

bc l = zipWith (+) (l++[0]) (0:l)
hasNoSquare n = all (\l->1==length l)$group$primeFactors n
e203 = sum$filter hasNoSquare$nub$concat$take 51$ iterate bc [1]
main = print $e203

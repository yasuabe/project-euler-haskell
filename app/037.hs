import Data.Numbers.Primes
import Data.List
isTruncatable prm = all (isPrime) sub
    where sub = map read$filter (not.null)$ (tails prm') ++ (inits prm')
          prm'= show prm
main = print$sum$take 11$filter isTruncatable $dropWhile (<=7) primes


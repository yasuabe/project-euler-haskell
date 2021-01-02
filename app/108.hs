import Data.List
import Data.Numbers.Primes

main = print $ find ((1000<).snd)$map (\n->(n,g n)) [1..]
  where
    f n = foldr (\xs n->n * (1 + 2*(length xs))) 1 $ group $ primeFactors n
    g n = div (1+(f n)) 2

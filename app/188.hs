import Data.Numbers.Primes
import Data.List

pow n 1 m = mod n m
pow n e m
  | even e    = mod (p'^2) m
  | otherwise = mod (n*p'^2) m
  where p'=pow n (div e 2) m

phi n = (div n (product pfs)) * foldr (\p s->(p-1)*s) 1 pfs
  where pfs = nub $ primeFactors n
        
tetrate a 1 _   = a
tetrate a n m = pow a n' m
  where n'= mod (tetrate a (n-1) m) p
        p = phi m

pe188 = tetrate 1777 1855 (10^8)

main = print $ pe188 

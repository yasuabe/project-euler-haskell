e3 = e3' 3 [] 600851475143 
  where
    e3' n pfs target 
        | product (n:pfs) == target    = n
        | 0 == target `mod` n &&
          all (\x-> 0 /= mod n x) pfs = e3' (n+2) (n:pfs) target
        | otherwise                   = e3' (n+2) pfs target
main = print e3
--6857        

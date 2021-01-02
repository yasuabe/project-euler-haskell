toSeptenary n = reverse $ f n
  where
   f m | q == 0    = [r]
       | otherwise = r : f q
    where (q, r) = divMod m 7

pe148 n = sum $ zipWith (\c e -> c * unit^e) coefs exps 
   where unit  = sum [1..7]
         sept  = toSeptenary n
         exps  = map ((length sept)-) [1..]
         sums  = map (\d -> sum [1..d]) sept
         prods = scanl (*) 1 $ map (+1) sept
         coefs = zipWith (*) sums prods

main = print $ pe148 (10^9)


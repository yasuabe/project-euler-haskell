-- need performance tuning
f m n | m==0      = 1
      | n==0      = 1
      | m<n       = f m m
      | otherwise = sum$map (\k->f (m-k) k)[1..n]
main =print$ f 100 99
-- $ ./76 +RTS -K500M -RTS
-- 190569291

--e75 0 = 1
--e75 1 = 1
--e75 n = sum$map (\k->(-1)^(k+1) * ((e75 (n-(div (k*(3*k-1)) 2)))+(e75 (n-(div (k*(3*k+1)) 2)))))[1..n]

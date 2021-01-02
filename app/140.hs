d = 5
r = 9
s = 4

f a1 a2 a3 a4 a5 a6 = a1 : f a2 a3 a4 a5 a6 (g a1)
g (x, q) = let (p', q') = (h.h) (p x, q) in (div (p'-7) 5, q')
  where
    h (p, q) = (r*p + d*s*q, s*p + r*q)
    p x = 5*x+7

main = print $ sum $ map fst $ take 30 
     $ f (2,7) (5,14) (21,50) (42,97) (152,343) (296,665)

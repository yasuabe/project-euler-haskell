cmb n m=div (product [n-m+1..n]) (product [1..m])
f s t i j = (cmb s i) * (cmb t j) * (2^j)
g n s t = sum $ map (\i->let j = n-i in f s t i j) [n - (min n t)..min n s]
h n = sum$map (\s->(s+1) * (g n s (24-s))) [0..24]
e158 = maximum$map h [1..24]

--ghci> e158

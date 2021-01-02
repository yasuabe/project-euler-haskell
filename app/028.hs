v 1 = 1
v n = (2* (tr (n-1)) + 5*(d n)) * 2
  where
    d  n' = (n'-1)*2
    tr 1 = 1
    tr n = (tr (n-1)) + ((d n)* 4)
main = print $ sum$map v [1..501]
--ghci> main
--669171001

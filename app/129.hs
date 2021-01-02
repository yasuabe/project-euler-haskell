solve (n:ns) | funcA n > 1000000 = n
             | otherwise         = solve ns
funcA n = f 1 1
  where f m k | rem == 0  = k
              | otherwise = f (rem*10+1) (k+1)
               where rem = m `mod` n

main = print $ solve $ scanl (+) 1000001 $ cycle [2,4,2,2]

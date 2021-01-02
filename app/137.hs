fib = 1:1:zipWith (+) fib (tail fib)
e137 = filter (\r->(fromIntegral$floor r)==r)
     $ map (\n->(sqrt (1+5*((fromIntegral n)^2-1))-1)/5) 
     $ drop 1$ fib
main = print$ceiling$e137!!15

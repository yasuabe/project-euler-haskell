digitSum = (sum.(map (\x->(read [x])::Int)).show)

e65 (x:[]) = (x, 1)
e65 (x:xs) = let (n, d) = e65 xs in (x * n + d, n)

main = print
     $ digitSum
     $ fst
     $ e65
     $ take 100
     $ 2: concat[[1,k,1]|k<-[2,4..]]

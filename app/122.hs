f xs@(xh:xt) = [(xh+x:xs)|x<-xs]
f1 xss = concatMap f xss
f2 n xss
  | null zs   = f2 n ys
  | otherwise = (length$head zs) - 1
  where ys = f1 xss
        zs = filter ((==n).head) ys
f3 1 = 0
f3 n = f2 n [[1]]

main = print $ sum $ map f3 [1..200]

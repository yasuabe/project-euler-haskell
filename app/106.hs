import Data.List

comb n m = div (product [m+1..n]) (product [1..(n-m)])

pairList n xs = [(zs, xs\\zs)|zs <- f1 n xs] where
  f1 n xs@(xh:xt)
    | n   == 1        = fmap (:[]) xs
    | length xs == n  = [xs]
    | otherwise       = [xh:ys| ys <- f1 (n-1) xt]
                        ++ f1 n xt

count m = (flip div 2) $ length $ select $ pairList m [1..(2*m)]
  where
    select pairs = filter (\(xs, ys)->f3
                 $ zipWith (\a b->a < b) xs ys) pairs
    f3 bs = (foldr (&&) True bs) /= (foldr (||) False bs)

euler106 n = sum $ map (f6 12) [2..div n 2]
  where f6 n m = (count m) * (comb n (2*m))

main = print $ euler106 12

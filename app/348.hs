import Control.Applicative

palindromics = concatMap f4 [1..] where
  f1  n = [10^(n-1)..10^n-1]
  f21 n = f3 (div n 10) n
  f22 n = f3 n n
  f4    = ((++) <$> (map f21) <*> (map f22)). f1
  f3  n result
    | n == 0     = result
    | otherwise  = f3 q (result * 10 + r)
    where (q, r) = divMod n 10

isMatched n      = f5 n 1 0 where
  f5 n m result 
    | n < m^3    = result
    | otherwise  = f5 n (succ m) (result+inc)
    where
      inc        = if isSquare (n - m^3) then 1 else 0
      isSquare n = (0<n) && (n==((^2).floor.sqrt.fromIntegral) n)

solve m n = sum $ take m $ filter ((n==).isMatched) palindromics

main = print $ solve 5 4

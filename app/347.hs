import Data.Numbers.Primes

f1 lmt (p:ps)
  | lmt <= p^2    = 0
  | otherwise     = (f2 lmt p ps) + (f1 lmt ps)
f2 lmt p (ph:pt)
  | lmt <= p*ph   = 0
  | otherwise     = (f3 lmt p p ph 0) + (f2 lmt p pt)
f3 lmt p px py total
  | lmt < px * py = 0
  | otherwise     = max (f4 lmt px py py total) (f3 lmt p (p*px) py total)
f4 lmt ph p pt total
  | lmt < ph*p*pt = max (ph*pt) total
  | otherwise     = f4 lmt ph p (p*pt) total

main = print $ f1 10000000 primes

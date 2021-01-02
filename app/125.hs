import Data.List

isPalindromic n = n == (read.reverse.show) n
f 0 _ c = c 
f l s c | 10^8 <= s' = c
        | otherwise  = f (l-1) s' c'
    where s' = s + l^2
          c' = if isPalindromic s' then (s':c) else c
g = concatMap (\l -> f (l-1) (l^2) []) [2..10^4]

main = print$sum$nub$g

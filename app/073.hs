import Data.Ratio
import Data.List

count d = count' start d 
  where start = if odd d then div2 else div2 -1 where div2 = div d 2
        count' n d | r <= (1%3) = 0
                   | d==denominator r = 1 + (count' (n-1) d)
                   | otherwise = (count' (n-1) d)
          where r = n%d
main = print$sum$map count [5..12000]

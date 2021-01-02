import Data.Numbers.Primes

f l c0 cnt | r < (0.1::Double) = 2*l-1
           | otherwise         = f (l+1) (c0+(2+8*(l-1))) cnt'
    where 
        cnt' = (cnt+) $ length $ filter isPrime $ map (\ci->c0+ci*(l-1)*2) [0..3]
        r    = (fromIntegral cnt') / (fromIntegral (1+4*(l-1)))

main = print $ f 2 3 0

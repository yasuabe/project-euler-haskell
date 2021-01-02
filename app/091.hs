import Data.Function
w = 50

x_l x1 y1 | w < y0    = (-(div (y1*y1-w*y1) (-x1))) + x1 
          | otherwise = 0
    where y0 = ((x1^2)///y1) + y1

x_r x1 y1 | yr < 0    = div (x1^2 + y1^2) x1
          | otherwise = w
    where yr = ((x1^2-w*x1)///y1) + y1

xrange x1 y1 = (x_l x1 y1, x_r x1 y1)

e91 x1 y1 = div (xr - xl - r) h
    where (xl, xr) = xrange x1 y1
          h = div y1 (gcd x1 y1)
          r = rem (x1-xl) h

(///) :: (Integral a)=> a->a->a
(///) a b = ceiling $ (on (/) fromIntegral) a b

main = print
     $ (sum$concatMap (\y->map (\x->e91 x y) [1..w]) [1..w])
       + w^2 * 3

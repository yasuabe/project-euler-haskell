import Data.Function

a = "1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679"
b = "8214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196"

f p a b
  | p <= a+b  = p -a
  | b == 1     = r
  | otherwise = if r <= 0 then r + b else r
  where r = (f p b (a+b)) - a

g (0, i) = a!!(i-1)
g (1, i) = b!!(i-1)

h :: [(Int,Int)]
h = map (\(a,b)->(on (,) fromInteger) a b)
  $ map (\n->let (d,m)=divMod n 100 in (f (d+1) 1 1, m))
  $ map (\n->(127+19*n)*7^n) [17,16..0]

pe230 = map g h

--ghci> pe230


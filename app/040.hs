-- would be more better solution
import Data.Char

pe040=map (\i->s!!(i-1)) [1,10,100,1000,10000,100000,1000000]
s = concat$map show ([1..200000])

main = print $ product $ map digitToInt $ pe040

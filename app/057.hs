denoms = 1:2:zipWith (\a b->a+2*b) denoms (tail denoms)
pe057 = zipWith (\a b->(f a) > (f b))
              (zipWith (+) denoms (tail denoms)) (tail denoms)
    where f = length.show

main = print
     $ length
     $ filter id
     $ take 1000
     $ pe057

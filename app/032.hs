import Data.List

digits = "123456789"
numbers x = map (\x->read x::Int) $permutations x
products (a, b, c) = [n*m|n<-numbers a, m<-numbers b, c==(sort.show) (n*m)]
triplets (len1, len2) =
    [(x,y,(digits\\x)\\y)|x<-subseqs len1 digits, y<-subseqs len2 $digits\\x]
subseqs len s = filter ((==len).length) $subsequences s

main = print
     $ sum $ nub
     $ concatMap (concatMap products.triplets) [(1,3),(1,4),(2,2),(2,3)]

--45228

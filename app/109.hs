import Data.List

ss = [1..20]++[25]
ds = map (*2) ss
ts = map (*3) [1..20]
sdts = sort$ss++ds++ts

n1 = length$ds
n2 = length$concat$map (\d->filter (<100)$map (\e->e+d) sdts ) ds
n3 = length$concat$concat$map (f sdts)$[99-x|x<-ds]
    where f l n = map (\t->takeWhile (\x->x<=(n-(head t))) t)
                $ takeWhile (\t->head t<=(n-(div n 2))) 
                $tails l
e109 = n1 + n2 + n3

--ghci> e109

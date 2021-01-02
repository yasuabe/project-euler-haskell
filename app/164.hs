import Data.List
import Data.Function

lm = 9
ini = [(t,1)|t<-[(f,s)|f<-[1..9],s<-[0..lm-f]]]

e164 ts = map (\l->(fst$head l, foldr (\(t,c) b->b + c) 0 l))
        $ groupBy (\a b->(on (==) fst) a b)$sort$concatMap f ts 
    where
        f ((a,b),c) = [((b,b2),c)|b2<-[0..lm-(a+b)]]

main = print 
     $ foldr (\(_,a) b->a+b)0$(!!18)$iterate e164 ini

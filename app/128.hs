import Data.Numbers.Primes
l1 = concat$scanl f ((0,0,1):(1,0,2):[]) [1..] where
    f ((_,_,n1):(l2,_,n2):[]) ln = ((l2,w-1,n1+w):(l2+1,0,n2+w):[])
                                 where w = ln*6
f1 l p n | n==1 || n==2 = 3
         | n==7         = 2
         | otherwise    = length$ filter (\v->isPrime$abs(v)) ds
  where
    ds | p==0      = [ln,  ln+1, 1,      -ln+6,ln-1,2*ln+5]
       | otherwise = [ln+6,-ln+1,-2*ln+7,-ln,  -1,  ln+5  ] 
       where ln = (l)*6
main = print $(filter ((3==).snd)
     $ map (\(l,p,n)->(n,f1 l p n)) l1)!!1999
--ghci> :main

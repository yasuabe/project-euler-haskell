import Data.List

p1 = 1+1+(sqrt 2)
f1 peri = floor (peri/p1)
f2 param@(a,b,peri,cnt) 
    | a==1            = param
    | peri' a b==peri = f2 (a-1,b,  peri,cnt+1)
    | peri' a b< peri = f2 (a,  b+1,peri,cnt)
    | peri' a b> peri = f2 (a-1,b,  peri,cnt)
    where peri' a b= a+b+(sqrt (a^2+b^2))
pe039 peri = f2 (a,a,peri,0) where a = fromIntegral (f1 peri)
main = print $ floor $ (\(_,_,ans,_)->ans)
     $ maximumBy (\(_,_,_,cnt1) (_,_,_,cnt2)->compare cnt1 cnt2)
     $ map pe039 [12,14..1000]


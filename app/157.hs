import Data.List
import Data.Numbers.Primes

-- e <- dividers of 10^(2n), e <= 10^n
divs10n n = map (\(t,f)->2^t*5^f)
          $ (\(h:t)->f3 n h ++ (concatMap f2 t))$f1 (n,n)
  where
    f1 p@(p2,p5) | 7<=p2     = p:plus3:plus2:plus1:[]
                 | 5<=p2     = p:plus2:plus1:[]
                 | 3<=p2     = p:plus1:[]
                 | otherwise = [p]
             where plus3=(p2-7,p5+3); plus2=(p2-5,p5+2); plus1=(p2-3,p5+1)
    f2 p@(p2,p5) | 0<p2       = p:f2 (p2-1,p5)
                 | otherwise  = [p]
    f3 lmt p@(p2,p5)
        | 0<p2  && 0<p5  = p:f2 (p2-1,p5) ++ f3 lmt (f6 lmt p)
        | 0==p2 && 0<p5  = p:f3 lmt (p2+2, p5-1)
        | 0<p2           = p:f3 lmt (p2-1,p5)
        | otherwise      = [p]
    f6 n p@(p2,p5) = (min (2*n) (p2+p2d),p5-1)
         where p2d = floor ((log (10^n/(2^p2*5^(p5-1))))/(log 2))

--common dividers of a and b
commonDividers (a, b) = dividers$group$cmn (primeFactors a) (primeFactors b)
  where
    cmn [] _        = []
    cmn _ []        = []
    cmn xs1@(xh1:xt1) xs2@(xh2:xt2)
        | xh1==xh2  = xh1:cmn xt1 xt2
        | xh1< xh2  = cmn xt1 xs2
        | otherwise = cmn xs1 xt2
    dividers (h:[]) = map product $ tails h
    dividers (h:t)  = sort$[(product l1)*l2|l1<-tails h, l2<-dividers t]


f157 m = length
       $ concatMap commonDividers 
       $ map(\n->((div (10^(m*2)) n) + 10^m, n + 10^m))
       $ divs10n m

main = print $sum$map f157 [1..9]

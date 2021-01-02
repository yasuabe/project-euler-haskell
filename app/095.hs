--need refactor
import Data.Numbers.Primes
import Data.List
import qualified Data.IntSet as S

lmt = 1000000
f1 n= map(\l->(head l, length l))$group$primeFactors n

f2 [] = [1]
f2 ((pr,pw):t) = [r*s|r<-map (pr^) [0..pw], s<-f2 t]

f3 n = (sum$f2 (f1 n)) - n

f4 o n h | lmt < s   = S.empty
         | s == 0    = S.empty
         | o == s    = (S.insert s h)
         | S.member s h  = S.empty
         | otherwise = f4 o s (S.insert s h)
    where s = f3 n

f5 mx _    []    = mx
f5 mx hist (h:t)
    | S.member h hist = f5 mx hist t
    | otherwise       = f5 (longer mx chain) (S.intersection chain hist) t
    where chain        = f4 h h S.empty
          longer l1 l2 = if S.size l1 < S.size l2 then l2 else l1

main = print$S.findMin$f5 S.empty S.empty [1..lmt]

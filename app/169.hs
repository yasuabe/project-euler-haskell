-- TODO: too slow

powers = map (2^) [0..83]
candidate n = f2'' n$takeWhile (<=n) $powers

f2 n (p1:p2:_) = f2' p1 $filter (\p'->((p1/=p2)||(p2/=p'))) $candidate n
f2 n (p1:_)    = f2' p1 $f2'' n$takeWhile (<=n) $ candidate n
f2 n _         = f2'' n$takeWhile (<=n) $candidate n

f2' l s = filter (<=l) s
f2'' n s  = filter (\x->(n-x)<=(x+(x-1)*2)) s

f3 0 s = 1
f3 r s = sum $map (\p'-> f3 (r-p') (p':s)) (f2 r s)

calcKs 0 = []
calcKs n = let (m, p) = maxK in m: calcKs (n - p) where
    maxK = last$takeWhile ((<=n).snd) $map (\m->(m,2^m)) [0..]
f (k1:[])    = k1 + 1
f (k1:k2:ks) = (k1-k2+1)*f (k2:ks) -1

--main = print $length$f3 (10^25) []
main = print$f3 (10^25) []

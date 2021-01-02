import qualified Data.Set as S
import Data.List

-- A100729
periods = [32, 26, 444, 1628, 5906, 80, 126960, 380882, 2097152]
-- A100730
diffs = [126, 126, 1778, 6510, 23622, 510, 507842, 1523526, 8388606]

ulam t l i n s
    | elem1       && elem2       = ulam t l (i) (n+2) s
    | (not elem1) && (not elem2) = ulam t l (i) (n+2) s
    | i == l                     = n
    | elem1       || elem2       = ulam t l (i+1) (n+2) (S.insert n s)
    where elem1 = S.member (n-2) s; elem2 = S.member (n-t) s

e167 n = k * (diffs!!!n) + (ulam t m i (t+1) s) where
    t = (n + 1) * 4
    i = n + 5
    j = (10^11 - i)::Integer
    k = div j (periods!!!n)
    l = mod j (periods!!!n)
    m = l + i
    s = S.fromList$2 : ([2*n+1, 2*n+3..(n+1)*4] ++ [(n+1) * 4])
    (!!!) ls n = ls!!((fromIntegral n)-2)

main = print$sum$Data.List.map e167 [2..10]

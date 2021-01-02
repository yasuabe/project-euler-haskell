--http://www004.upp.so-net.ne.jp/s_honma/pell/pell.htm
import Data.List
import Data.Function

isNotSquare n= (round . sqrt $ fromIntegral n) ^ 2 /= n
pell d=pell' d (0,0,0,0,1) (0,1,floor(sqrt(fromIntegral d)),1,0)
  where
   pell' d (g,h,k,x,y) n@(gn,hn,kn,xn,yn)
    | g'== gn    = let xr = div (xn^2 + d*yn^2) hn
                       yr = div (2*xn*yn) hn
                   in (xr, yr)
    | h'== hn    = let xr = div (x'*xn + d*y'*yn) hn
                       yr = div (x'*yn + xn*y') hn
                   in (xr^2 + d*yr^2, 2*xr*yr)
    | otherwise  = pell' d n (g',h',k',x',y')
    where k0 = floor$sqrt$fromIntegral d
          g' = (-gn) + kn*hn
          h' = div (d - g'^2) hn
          k' = div (k0 + g') h'
          y' = y + kn*yn
          x' = g'*y' + h'*yn
e66 = maximumBy (\a b->(on compare (snd.snd))a b )
    $ map (\n->(n,pell n)) $filter isNotSquare [2..1000]

main = print $ fst $ e66

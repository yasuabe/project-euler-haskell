import Control.Monad.ST
import Data.STRef
import Data.List

e104 :: Integer 
e104 = runST $ newSTRef (0, 1, 0, 1) >>= f 0 where
    f n r = do
        (xl, yl, xr, yr) <- readSTRef r
        let (xl', yl') = if (10^18) < xl && (10^18)< yl
                         then (div xl 10, div yl 10) else (xl, yl)
        let (xr', yr') = (mod xr (10^9), mod yr (10^9))
        writeSTRef r (yl', xl'+yl', yr', xr'+yr')
        if matches xl xr then return n
                         else f (n+1) r
    matches nl nr = (has1to9.(take 9).show) nl && (has1to9.show) nr
    has1to9 s     = "123456789" == (sort s)

main = print e104

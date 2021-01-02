import Data.List

isTn n = d==fromIntegral(floor d)
    where d=(-1+(sqrt(1+8*(fromIntegral n))))/2
isPn n = d==fromIntegral(floor d)
    where d=(1+(sqrt(1+24*(fromIntegral n))))/6
h n = n*(2*n-1)

main = do let Just ans = find (\n->isTn n&&isPn n)$map h [144..] 
          print ans


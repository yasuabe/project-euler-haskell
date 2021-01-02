import Data.List

p n = div (n*(3*n-1)) 2
pair n1 n2 = (d,isPn d,isPn s) where d=(p n2)-(p n1); s=(p n2)+(p n1)
isPn n = d==fromIntegral(floor d)
    where d=(1+(sqrt(1+24*(fromIntegral n))))/6
search m = map (\n->pair n m) [1..(m-1)]

main = do let Just (ans,_,_) = find (\(d,f,s) -> f&&s)
                             $ concatMap search [1..]
          print ans

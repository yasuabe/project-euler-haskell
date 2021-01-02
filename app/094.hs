--need refactor
isSquare :: Integral a => a -> Bool
isSquare n = (round$sqrt$fromIntegral n)^2 == n
main = print$(sum$map(\m2->4*m2)$filter(\m2->let n2=div (m2-1) 3 in 0/=n2 &&isSquare n2)
                                $filter (\m2-> 0==mod (m2-1) 3)
                                $takeWhile (<div (10^9) 4) $map (^2) [1..]) +
             (sum$map(\l->2*l^2)$filter (\l-> any (\m->let n=l-m in 0==m*m-4*m*n+n*n-1) [1..l-1]) [1..22360])

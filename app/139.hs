-- TODO: refactor
pyt1 (a,b,c) = ((-1)*a+(-2)*b+2*c,(-2)*a+(-1)*b+2*c,(-2)*a+(-2)*b+3*c)
pyt2 (a,b,c) = [pyt1(-a,b,c), pyt1(-a,-b,c), pyt1(a,-b,c)]
pyt3 ts = ts ++ pyt3 (concatMap pyt2 ts)
e139 = map(\n->div (10^8) n)$takeWhile(<10^8)$map (\(a,b,c)->a+b+c)$filter(\(a,b,c)->0==mod c (a-b))$pyt3 [(3,4,5)]
main = print$sum$e139

pyt1 (a,b,c) = ((-1)*a+(-2)*b+2*c,(-2)*a+(-1)*b+2*c,(-2)*a+(-2)*b+3*c)
pyt2 (a,b,c) = [pyt1(-a,b,c), pyt1(-a,-b,c), pyt1(a,-b,c)]
pyt3 ts = ts ++ pyt3 (concatMap pyt2 ts)

e138_1 = take 7$filter(\(a,b,c)->1==abs(2*b-a)||1==abs(2*a-b))$pyt3 [(3,4,5)]
 
l138 = 17:305:zipWith (\b a->18*a-b) l138 (tail l138)
main = print$sum$take 12$l138

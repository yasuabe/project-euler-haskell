sqrti = sqrt.fromIntegral
e64 n = map (\(_,_,c)->c) $tail $e64' [(0,1,m)] where
    m = floor$sqrti n
    e64' hist@((a,b,c):xs) =
        let r = (a',b',c')
        in if elem r hist then reverse hist
                      else e64' (r:hist)
      where
        a'=b*c-a
        b'=div (n-a'*a') b
        c'=div (m+a') b'

main = print
     $ length
     $ filter odd
     $ map (\n->(length.e64) n) 
     $ filter (\n->((round.sqrti) n)^2 /= n) [1..10000]

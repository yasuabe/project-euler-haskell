--ghci> readFile "files/base_exp.txt">>=(\cnt->print $maximumBy (\(_,_,_,a) (_,_,_,b)->compare a b)$map (\(n,(base,exp))->(n,base, exp,exp*(log base)))$map (\(n,l)->(n,f l))$zipWith (\n l->(n,l))[1..]$lines cnt)
--(709,895447.0,504922.0,6919995.552420337)


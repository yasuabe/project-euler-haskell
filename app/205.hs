f col l = let zeros = replicate (col-1) 0
              l'    = zeros ++ l ++ zeros in
    map (\n->sum$take col$drop n l') [0..(length l')-col]

peter = map (/(4^9))$(iterate (f 4) [1,1,1,1])!!8
colin = map (/(6^6))$(iterate (f 6) [1,1,1,1,1,1])!!5

p n = peter!!n * (sum$take (n+3) colin)

main = print$sum$map p [0..27]

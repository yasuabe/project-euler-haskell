main = print
     $ sum
     $ map(\n->9-n+1)
     $ takeWhile (<10)
     $ map lowerLimit [1..]
    where lowerLimit n = ceiling((10**(n-1))**(1/n))

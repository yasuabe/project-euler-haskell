isPalind n = s==(reverse s) where s=show n
rev n = (read::String->Integer)$reverse $show n
revAdd n = n+(rev n)
e55 n = e55' n 1 where
    e55' n 50 = -1
    e55' n c  = if isPalind n' then c else e55' n' (c+1) where n'=revAdd n

main = print
     $ length
     $ filter (==(-1))
     $ map e55 [1..10000]

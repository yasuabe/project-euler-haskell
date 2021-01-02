import System.IO
import Data.List
import Data.List.Utils

main = do 
    h <- openFile "files/roman.txt" ReadMode
    contents <- hGetContents h
    print $sum$map (\s->length s-length (shorten s))$lines contents
    hClose h

shorten s = h (f 1000 s) 1
               
f u []      = 0
f u s@(x:xs) =
  let (vs, def) = conv u; v = lookup x vs 
  in case v of
    Just v' -> f'' v'
    Nothing -> f def s
  where
    f'' a = a * u + (f u xs)
    conv 1000 = ([('M',1)],500)
    conv  500 = ([('D',1)],100)
    conv  100 = ([('M',8), ('D',3), ('C',1)],50)
    conv   50 = ([('L',1)],10)
    conv   10 = ([('C',8), ('L',3), ('X',1)],5)
    conv    5 = ([('V',1)],1)
    conv    1 = ([('X',8), ('V',3), ('I',1)],50)


h 0 p = []
h n p = (h (div n 10) (p+1)) ++ (h' (mod n 10) p)
h' n p
        | p == 1 && n == 0 = []
        | p == 1 && n <  4 = take n$repeat 'I'
        | p == 1 && n == 4 = "IV"
        | p == 1 && n == 5 = "V"
        | p == 1 && n <  9 = 'V':(take (n-5)$repeat 'I')
        | p == 1 && n == 9 = "IX"
        | p == 2 && n == 0 = []
        | p == 2 && n <  4 = take n$repeat 'X'
        | p == 2 && n == 4 = "XL"
        | p == 2 && n == 5 = "L"
        | p == 2 && n <  9 = 'L':(take (n-5)$repeat 'X')
        | p == 2 && n == 9 = "XC"
        | p == 3 && n == 0 = []
        | p == 3 && n <  4 = take n$repeat 'C'
        | p == 3 && n == 4 = "CD"
        | p == 3 && n == 5 = "D"
        | p == 3 && n <  9 = 'D':(take (n-5)$repeat 'C')
        | p == 3 && n == 9 = "CM"
        | p == 4 && n == 0 = []
        | p == 4           = take n $repeat 'M'

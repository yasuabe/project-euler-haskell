import Data.List.Split

sameSide x1 y1 x2 y2 x3 y3 =
    0 <= (d x3 y3) * (d 0 0)
    where d x y = (x1-x2)*(y-y1)+(y1-y2)*(x1-x)

includes (x1:y1:x2:y2:x3:y3:[]) =
    sameSide x1 y1 x2 y2 ax ay &&
    sameSide x2 y2 x3 y3 ax ay &&
    sameSide x3 y3 x1 y1 ax ay 
    where ax = (x1 + x2 + x3) / 3
          ay = (y1 + y2 + y3) / 3

e102 cnt = length$filter f $lines cnt where
     f l = includes $ map (\w->(read w)::Double)$ splitOn "," l

main = readFile "files/triangles.txt">>=(print.e102)

import Data.String.Utils

isTriangleNumber w = d==fromIntegral(floor d)
    where d=sqrt(1+8*(fromIntegral n))
          n=sum$map ((flip (-) 64).fromEnum) w

theWords = do
    cnt<-readFile "files/p042_words.txt"
    return $map ((\s->take ((length s)-1) s).(drop 1))$split "," cnt

pe042 = do ws<-theWords
           return$length$filter (isTriangleNumber) ws
main = pe042 >>= print

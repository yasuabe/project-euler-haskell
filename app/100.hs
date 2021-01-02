import Data.List
import Data.Maybe

--http://www.johnsasser.com/pdf/article15.pdf
f :: Integer->Integer->[(Integer,Integer)]
f alpha beta = (a,a+y): (f alpha' beta') where
        alpha' = 3*alpha+4*beta
        beta'  = 2*alpha+3*beta
        a  = div (beta' + 1) 2
        z' = (alpha', beta',a,a+y)
        y = let b = 2*a-1; c = a^2-a
            in div ((-b)+(sqrt' (b^2+4*c))) 2
        sqrt' r = ceiling$sqrt$fromIntegral r

main = print $ fst $ fromJust $ find (\(_,t)->10^12<t)$f 1 1

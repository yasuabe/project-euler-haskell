import Data.List
import Data.Maybe

numWords = [
    (1, "one"),
    (2, "two"),
    (3, "three"),
    (4, "four"),
    (5, "five"),
    (6, "six"),
    (7, "seven"),
    (8, "eight"),
    (9, "nine"),
    (10, "ten"),
    (11, "eleven"),
    (12, "twelve"),
    (13, "thirteen"),
    (14, "fourteen"),
    (15, "fifteen"),
    (16, "sixteen"),
    (17, "seventeen"),
    (18, "eighteen"),
    (19, "nineteen"),
    (20, "twenty"),
    (30, "thirty"),
    (40, "forty"),
    (50, "fifty"),
    (60, "sixty"),
    (70, "seventy"),
    (80, "eighty"),
    (90, "ninety"),
    (100, "hundred"),
    (1000, "thousand")]
len n = length$fromJust$lookup n numWords


oneTo19      = sum $map len [1..19]
twentyTo99   = sum $map (\n->10 * (len (n*10)) + (sum$map len [1..9])) [2..9]
one100To999  = sum $map (\n->(len n + len 100)*100+3*99+oneTo19+twentyTo99) [1..9]
oneThousand  = len 1 + len 1000

main = print $ sum [oneTo19, twentyTo99, one100To999, oneThousand]
--ghci> :main
--21124
--


g (1,   pre) l = pre:l
g (cur, 1)   l = 1:(cur-1):l
g (cur, pre) l 
    | cur<pre = g (cur, mod pre cur) (div pre cur:l)
    | cur>pre = g (mod cur pre, pre) (div cur pre:l)
main = print$ g (13717421, 109739369) []


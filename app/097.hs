f w 0   ans = ans
f w rep ans = f w (rep-1) (mod (2*ans ) (10^w))
main= print $ mod ((f 6 7830457 1)*28433+1) (10^10)

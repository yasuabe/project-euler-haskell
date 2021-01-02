divisors n = [x | x <- [1..(n-1)], n `rem` x == 0]
isAmicable n = n == sumOfDivisors n' && n /= n' where
           n' = sumOfDivisors n
           sumOfDivisors n = sum$divisors n

pe020 = filter isAmicable [1..10000]
main = print $sum pe020
-- $./21 +RTS -K500M -RTS
--31626
--

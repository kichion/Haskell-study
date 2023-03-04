myLength [] = 0
-- myLength xs = 1 + myLength (tail xs)
myLength (_ : xs) = 1 + myLength xs

myTake _ [] = []
myTake 0 _ = []
myTake n (x : xs) = x : rest
  where
    rest = myTake (n - 1) xs

finiteCycle (first : rest) = first : rest ++ [first]

myCycle (first : rest) = first : myCycle (rest ++ [first])

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) m
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

collatz 1 = 1
collatz n =
  if even n
    then 1 + collatz (n `div` 2)
    else 1 + collatz (n * 3 + 1)

myReverse [] = []
myReverse (x : []) = [x]
myReverse (x : xs) = (myReverse xs) ++ [x]

firstFib _ _ 0 = 1
firstFib _ _ 1 = 2
firstFib _ _ 2 = 3
firstFib n1 n2 3 = n1 + n2
firstFib n1 n2 counter = firstFib (n1 + n2) (n1) (counter - 1)
-- firstFib 1 1 5 = firstFib (1 + 1) (1) (4)
-- firstFib 2 1 4 = firstFib (2 + 1) (2) (3)
-- firstFib 3 2 3 = 3 + 2
-- O(n-2)

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
-- fib 5 = fib (4) + fib (3)
-- 1. fib (4) = fib (3) + fib (2)
-- 1-1. fib (3) = fib (2) + fib (1)
-- 1-1-1. fib (2) = fib (1) + fib (0)
-- 1-2. fib (2) = fib (1) + fib (0)
-- 2. fib (3) = fib (2) + fib (1)
-- 2-1. fib (2) = fib (1) + fib (0)
-- O(n*3 - 1)

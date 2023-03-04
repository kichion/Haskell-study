myTail (_ : xs) = xs
myTail [] = []

-- myGCD a b =
--   if remainder == 0
--     then b
--     else myGCD b remainder
--   where
--     remainder = a `mod` b

myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b)

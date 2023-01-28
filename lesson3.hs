doubleDouble x = (* 2) x * 2

plusTest x = (+ 5) x * 2

overwrite x =
  let x = 2
   in let x = 3
       in let x = 4
           in x

overwrite2 x = (\x -> (\x -> (\x -> x) 4) 3) 2

counter x = (\x -> (\x -> (\x -> x) x + 1) x + 1) x

inc = (\n -> n + 1)

double = (\n -> n * 2)

square = (\n -> n ^ 2)

test n =
  if e
    then n - 2
    else 3 * n + 1
  where
    e = even n

inc n = n + 1

double n = n * 2

square n = n ^ 2

test n =
  if e
    then n - 2
    else 3 * n + 1
  where
    e = even n

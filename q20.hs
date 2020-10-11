

g x | x == 0 = 3
    | x == 1 = 1
    | x >= 2 = gx
    where
      gx = g (x - 1) + 3 * g (x - 2)
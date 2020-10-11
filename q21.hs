

h n | n == 0 = 3
    | n == 1 = 1
    | n >= 2 = let
        hn = 2*h (n - 1) + 3*h (n - 2)
      in hn
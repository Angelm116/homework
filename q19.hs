
m, n :: (Num a) => [a] -> a
m []     = 1
m (x:xs) = x * (m xs)

n []     = 2
n (x:xs) = x * (n xs)

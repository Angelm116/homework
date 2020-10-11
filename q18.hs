
maxEvenOdd :: Integral a => [a] -> Int
maxEvenOdd ls = max (length $ (filter even) ls) (length $ (filter odd) ls) 
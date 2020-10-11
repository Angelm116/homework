
-- Q17

tell :: (Show a) => [a] -> String
tell l
  | null l           = "Empty"
  | (length l) == 1  = "Contains only 1 item: " ++ show (head l)
  | (length l) == 2  = "Contains only 2 items: " (show (head l)) ++ " and " ++ (show (head (tail l)))
  | otherwise        = "Contains many items... " ++ (show l)


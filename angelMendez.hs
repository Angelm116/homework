
-- Q1

-- Recursive
delete_all :: (Eq a) => a -> ([a] -> [a])
delete_all _ [] = []
delete_all x (y:ys)
  | x == y    = delete_all x ys
  | otherwise = y : delete_all x ys

-- List comp
delete_all' :: (Eq a) => a -> ([a] -> [a])
delete_all' x ys = [y | y <- ys, y /= x]

-- Prelude functions
delete_all'' :: (Eq a) => a -> ([a] -> [a])
delete_all'' x ys = filter (/= x) ys

-- Q2

-- Recursive
del_fstnscnd :: (Eq a) => a -> ([a] -> [a])
del_fstnscnd x ys = d x ys 2
  where
    d _ ys 0 = ys
    d _ [] _ = []
    d x (y:ys) n 
      | x == y    = d x ys (n-1)
      | otherwise = y : d x (ys) n

-- It can't be done with list comprehensions because it takes into account
-- occurrences and how many of them

-- Prelude Functions
del_fstnscnd' :: (Eq a) => a -> ([a] -> [a])
del_fstnscnd' x ys = snd (foldl d (2,[]) ys)
  where
    d (0,t) y = (0,t ++ [y])
    d (n,t) y
      | x == y    = (n-1,t)
      | otherwise = (n, t ++ [y])

-- Q3

merge :: (Ord a) => [[a]] -> [a]
merge []  = []
merge [t] = t
merge (t:u:ls) = merge ((merge2 t u):ls)
  where
    merge2 [] xs = xs
    merge2 ys [] = ys
    merge2 (x:xs) (y:ys)
      | x < y     = x:(merge2 xs (y:ys))
      | otherwise = y:(merge2 (x:xs) ys)

mergeU :: (Ord a) => [[a]] -> [a]
mergeU []  = []
mergeU [t] = t
mergeU (t:u:ls) = mergeU ((merge2 t u):ls)
  where
    merge2 [] xs = xs
    merge2 ys [] = ys
    merge2 (x:xs) (y:ys)
      | x < y     = x:(merge2 xs (y:ys))
      | x == y    = x:(merge2 xs    ys )
      | otherwise = y:(merge2 (x:xs) ys)

-- Q4

sum_votes_if :: String -> [(String, Int)] -> (Int, Bool)
sum_votes_if person votes = let
    counted_votes = merge_votes' (map (:[]) votes)
    winner_votes  = maximum $ map snd $ counted_votes
    person_votes  = maybe 0 id (lookup person counted_votes)
  in
    (person_votes, person_votes == winner_votes)

merge_votes' :: [[(String,Int)]] -> [(String,Int)]
merge_votes' []  = []
merge_votes' [v] = v
merge_votes' (t:u:ls) = merge_votes' ((merge2 t u):ls)
  where
    merge2 [] xs = xs
    merge2 ys [] = ys
    merge2 ((n, v):xs) ((n', v'):ys)
      | n < n'    = (n,v):(merge2 xs ((n',v'):ys))
      | n == n'   = (n, v+v'):(merge2 xs ys)
      | otherwise = (n',v'):(merge2 ((n,v):xs) ys)

-- Q5

sumHarmonic :: (Fractional a, Eq a) => a -> a
sumHarmonic x = sh 1 x
  where
    sh y x 
      | y == x    = (1/y)
      | otherwise = (1/y) + (sh (y+1) x)

-- Q6

evaluateFunc :: Int -> Bool
evaluateFunc m = let
    m'        = fromIntegral m
    leftSide  = sum [ x**3 | x <- [1..m']]
    rightSide = ((m' * (m' + 1)) / 2) ** 2
  in leftSide == rightSide

-- map evaluateFunc [1..20] == [True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]

-- Q7

string2word :: String -> [String]
string2word str = stw str [""]
  where
    stw [] ("":w:words) = reverse words
    stw [] words        = reverse words
    stw (ch:str) ("":words)
      | (ch == ' ') = stw str (""  :words)
      | otherwise   = stw str ([ch]:words) 
    stw (ch:str) (w:words)
      | (ch == ' ') = stw str ("":w:words)
      | otherwise   = stw str ((w ++ [ch]):words)

-- Q8

indexof :: (Eq t) => t -> [t] -> Int
indexof x l = io x l 0
  where
    io x [] _ = -1
    io x (y:ys) i
      | x == y    = i
      | otherwise = io x ys (i+1)

-- Q9

approximations :: (Real a, Fractional a) => a -> a -> [a]
approximations s x = iterate heron x 
  where
    heron x = (s/x + x)/2

-- Q10

composeFunc:: [(a -> a)] -> (a -> a)
composeFunc fs = foldl (.) id fs

-- Q11

quicksort :: (Ord a) => [a] -> [a]
quicksort []     = []
quicksort (x:ls) = quicksort [l | l <- ls , l <= x] ++ [x] ++ quicksort [r | r <- ls, r > x] 

readCard :: String -> (Char, Int)
readCard str = ((last str), read (init str))

validCard :: (Char, Int) -> Bool
validCard (c, i) = c `elem` "RGBY" && i `elem` [1..10]

colorScore :: [(Char, Int)] -> Int
colorScore cards = 
  sum $ map awardPoint $ (groupOcc []) $ quicksort $ (map fst) cards
    where
      groupOcc acc          []     = acc 
      groupOcc []           (y:ys) = groupOcc [[y]] ys
      groupOcc ((x:xs):xss) (y:ys) 
        | y == x    = groupOcc ((y:x:xs):xss) ys
        | otherwise = groupOcc ([y]:(x:xs):xss) ys

      awardPoint l
        | (length l) == 2 = 1
        | (length l) == 3 = 2 
        | (length l) == 4 = 5
        | otherwise       = 0 

numScore :: [(Char, Int)] -> Int
numScore cards =
  sum $ map awardPoint $ consecutive [] $ quicksort $ map snd cards
    where
      consecutive acc []    = acc
      consecutive [] (y:ys) = consecutive [[y]] ys
      consecutive ((x:xs):xss) (y:ys)
        | (y - 1) == x = consecutive ((y:x:xs):xss) ys
        | otherwise    = consecutive ([y]:(x:xs):xss) ys

      awardPoint l
        | (length l) == 2 = 2
        | (length l) == 3 = 3 
        | (length l) >= 4 = 5
        | otherwise       = 0 

score :: [(Char, Int)] -> Int
score cards = (colorScore cards) + (numScore cards)

validHand :: (String, [String]) -> Bool
validHand (player, hand) = player `elem` ["A","B","C"] && (length hand) == 4 

validGame :: [(String, [String])] -> Bool
validGame hands = (length hands) == 3

winner :: [(String, [String])] -> (String, Int)
winner hands 
  | not (validGame hands)       = error "Invalid game."
  | not vh                      = error "Invalid hand size or player."
  | not vc                      = error "Invalid card."
  | otherwise                   = (\(b,a) -> (a,b)) $ maximum $ zip scores players
  where
    vh      = and (map validHand hands)
    players = map fst hands
    cards   = map ((map readCard) . snd) hands
    vc      = and $ concatMap (map validCard) cards
    scores  = map score cards


-- Normal win
test1 = winner [
      ("A", ["2R", "3R", "4R", "6B"])
    , ("B", ["2Y", "4B", "6Y", "8R"])
    , ("C", ["1Y", "3B", "5Y", "7R"])
  ] == ("A", 5)

-- Invalid game, too many players
test2 = winner [
      ("A", ["2R", "3R", "4R", "6B"])
    , ("B", ["2Y", "4B", "6Y", "8R"])
    , ("C", ["1Y", "3B", "5Y", "7R"])
    , ("D", ["3Y", "5B", "7Y", "9R"])
  ]

-- Invalid game, wrong player name
test3 = winner [
      ("A", ["2R", "3R", "4R", "6B"])
    , ("B", ["2Y", "4B", "6Y", "8R"])
    , ("D", ["1Y", "3B", "5Y", "7R"])
  ]

-- Invalid card, wrong color
test4 = winner [
      ("A", ["2R", "3R", "4R", "6B"])
    , ("B", ["2Y", "4B", "6Y", "8R"])
    , ("C", ["1V", "3B", "5Y", "7R"])
  ] 

-- Invalid card, wrong number
test5 = winner [
      ("A", ["2R", "3R", "4R", "6B"])
    , ("B", ["12Y", "4B", "6Y", "8R"])
    , ("C", ["1Y", "3B", "5Y", "7R"])
  ] 

-- Win by color
test6 = winner [
      ("A", ["1R", "3R", "5R", "9R"])
    , ("B", ["2Y", "4B", "6Y", "8R"])
    , ("C", ["1Y", "3B", "5Y", "7R"])
  ] == ("A", 5)

-- Win by numbers
test7 = winner [
      ("A", ["2Y", "4B", "6Y", "8R"])
    , ("B", ["1R", "2B", "3Y", "4G"])
    , ("C", ["1Y", "3B", "5Y", "7R"])
  ] == ("B", 5)

-- Max points 
test8 = winner [
      ("A", ["2Y", "4B", "6Y", "8R"])
    , ("B", ["1Y", "3B", "5Y", "7R"])
    , ("C", ["1R", "2R", "3R", "4R"])
  ] == ("C", 10)

-- Data type set implemented as an ordered map.
-- The function insert ensures that the set is well ordered and that it 
-- only has a single instance of each value.

-- E represents the empty set while S x ss represents an element x in the
-- set with ss as the next element on the set.
data Set a = S a (Set a)
           | E
           deriving (Eq, Ord)

-- Instance of the class Show for a set. Uses the function showSet.
instance Show a => Show (Set a) where
  show s = showSet show s

-- Instance of the class Foldable for reducing a set into a single value
-- using a "plus" function.
instance Foldable Set where
  foldr plus acc E        = acc
  foldr plus acc (S a ss) = foldr plus (plus a acc) ss

-- Empty set
empty :: Set a
empty = E

-- Set with only the given element
singleton :: a -> Set a
singleton a = S a E

-- Function that properly adds an element to a set. Ignores repeated elements
-- and enters them in order so transversing over th set is easier.
insert :: Ord a => Set a -> a -> Set a
insert E a = S a E
insert (S a ss) a'
  | a' >  a = S a (insert ss a')
  | a' == a = S a ss
  | a' <  a = S a' (S a ss)

-- Turns a list into a set using the function insert.
listToSet :: Ord a => [a] -> Set a
listToSet = foldl insert E

-- Turns a set into a list usign the set's foldl function.
setToList :: Ord a => Set a -> [a]
setToList = foldl (flip (:)) []

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

-- Checks if an element is a member of the given set.
memberOfSet :: Ord a => Set a -> a -> Bool
memberOfSet E a = False
memberOfSet (S a ss) a'
  | a' >  a = memberOfSet ss a'
  | a' == a = True
  | a' <  a = False

-- Merges 2 sets into one.
union :: Ord a => Set a -> Set a -> Set a
union E s = s
union s E = s
union l@(S a ss) r@(S a' ss')
  | a > a'  = S a' (union l ss' )
  | a < a'  = S a  (union ss  r )
  | a == a' = S a  (union ss ss')

-- Returns the common elements between two sets.
intersection :: Ord a => Set a -> Set a -> Set a
intersection E s = E
intersection s E = E
intersection l@(S a ss) r@(S a' ss')
  | a > a'  =      intersection l  ss'
  | a < a'  =      intersection ss r
  | a == a' = S a (intersection ss ss')

-- Returns every element on a set that is not part of a second one.
difference :: Ord a => Set a -> Set a -> Set a
difference E s = E
difference s E = s
difference l@(S a ss) r@(S a' ss')
  | a > a'  =      difference l  ss'
  | a < a'  = S a (difference ss r  )
  | a == a' =      difference ss ss'

-- Checks if 2 sets have the same elements.
equalSet :: Eq a => Set a -> Set a -> Bool
equalSet = (==)

-- Checks if a set is completely part of another.
subSet :: Ord a => Set a -> Set a -> Bool 
subSet ss = foldl (\acc e -> acc && (memberOfSet ss e)) True

-- Applies the given function to all elements of a set.
mapSet :: Ord b => (a -> b) -> Set a -> Set b
mapSet f E        = E
mapSet f (S a ss) = insert (mapSet f ss) (f a)

-- Given a predicate returns a set with all elements that satisfy it.
filterSet :: (a -> Bool) -> Set a -> Set a
filterSet _ E = E
filterSet p (S a ss)
  | p a       = S a (filterSet p ss) 
  | otherwise =      filterSet p ss

-- Reduces a set into a single value given a function.
foldSet :: (a -> a -> a) -> a -> Set a -> a
foldSet = foldl

-- Shows a string representation of a set.
showSet :: (a -> String) -> Set a -> String
showSet f s = "{" ++ (show' s)
  where
    show' E        = "}"
    show' (S a E ) = f a ++ show' E  
    show' (S a ss) = f a ++ "," ++ show' ss

--------------------------------------------------------------------------------
-- Test Cases
--------------------------------------------------------------------------------

tests :: [String]
tests = [ tempty
        , tmemberOfSet
        , tunion
        , tintersection
        , tdifference
        , tequalSet
        , tsubSet
        , tmapSet
        , tfilterSet
        , tfoldSet
        ]  

tempty :: String
tempty = let
    test    =  empty == (E :: Set Int) 
    message = "empty == E" 
  in
    message ++ " is: " ++ (show test)

tmemberOfSet :: String
tmemberOfSet = let
    test1    = (memberOfSet (listToSet [1,2,3]) 3) == True
    message1 = "memberOfSet {1,2,3} 3 == True" 
    test2    = (memberOfSet (listToSet []) 3) == False
    message2 = "memberOfSet {} 3 == False" 
  in
    message1 ++ " is: " ++ (show test1) ++ "\n" ++
    message2 ++ " is: " ++ (show test2)

tunion :: String
tunion = let
    test1 = (listToSet [1,2,3]) `union` (listToSet [2,3,4])
    message1 = "{1,2,3} `union` {2,3,4}"
    test2    = (listToSet [1,2,3]) `union` (listToSet [])
    message2 = "{1,2,3} `union` {}" 
  in
    message1 ++ " is: " ++ (show test1) ++ "\n" ++
    message2 ++ " is: " ++ (show test2)

tintersection :: String
tintersection = let
    test1 = (listToSet [1,2,3]) `intersection` (listToSet [2,3,4])
    message1 = "{1,2,3} `intersection` {2,3,4}"
    test2    = (listToSet [1,2,3]) `intersection` (listToSet [])
    message2 = "{1,2,3} `intersection` {}" 
  in
    message1 ++ " is: " ++ (show test1) ++ "\n" ++
    message2 ++ " is: " ++ (show test2)

tdifference :: String
tdifference = let
    test1 = (listToSet [1,2,3]) `difference` (listToSet [2,3,4])
    message1 = "{1,2,3} `difference` {2,3,4}"
    test2    = (listToSet [1,2,3]) `difference` (listToSet [])
    message2 = "{1,2,3} `difference` {}" 
  in
    message1 ++ " is: " ++ (show test1) ++ "\n" ++
    message2 ++ " is: " ++ (show test2)

tequalSet :: String
tequalSet = let
    test = ((listToSet [1,2,2,3]) `equalSet` (listToSet [3,2,1])) == True
    message = "({1,2,2,3} == {3,2,1}) == True"
  in
    message ++ " is: " ++ (show test)

tsubSet :: String
tsubSet = let
    test = ((listToSet [2,3,4,5]) `subSet` (listToSet [3,2,2])) == True
    message = "{2,3,4,5} contains {3,2,2} == True"
  in
    message ++ " is: " ++ (show test)

tmapSet :: String
tmapSet = let
    test = mapSet (^2) (listToSet [-2, 2, 4, 3.5])
    message = "mapSet (^2) {-2,2,4,3.5}"
  in
    message ++ " is: " ++ (show test)

tfilterSet :: String
tfilterSet = let
    test = filterSet (> 5) (listToSet [2,4,6,7,7,6,7,8,1,2,3,4])
    message = "filterSet (>5) {2,4,6,7,7,6,7,8,1,2,3,4}" 
  in
    message ++ " is: " ++ (show test)

tfoldSet :: String
tfoldSet = let
    test = foldSet (+) 0 (listToSet [1,1,1,2,2,2,2,3,3,3,3,3,4,4,4])
    message = "foldSet (+) {1,1,1,2,2,2,2,3,3,3,3,3,4,4,4}"
  in
    message ++ " is: " ++ (show test)

main :: IO ()
main = do
  putStrLn "Test cases for the set module:\n" 
  putStr  $ unlines $ map (++ "\n") tests
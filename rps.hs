
-- Rock-Paper-Scissors Tournament.

-- Data type for a move in the Rock-Paper-Scissors Game.
data Move = Rock 
          | Paper 
          | Scissors 
            deriving (Eq, Show)

-- Functions for rapidly returning the opposing move.
beat :: Move -> Move
beat Rock     = Paper
beat Paper    = Scissors
beat Scissors = Rock

lose :: Move -> Move
lose Rock     = Scissors
lose Paper    = Rock
lose Scissors = Paper

-- Collection of moves that 2 players have done in a game in order.
type Tournament = ([Move], [Move])

-- Gives an outcome equal to 1, 0 or -1 according to whether the first Move 
-- beats the second Move.
outcome :: Move -> Move -> Integer
outcome m m' 
  | m == beat m' =  1
  | m == lose m' = -1
  | otherwise    =  0

-- Given a Tournament, returns the sum of the score that the first list of 
-- moves got. The score is calculated through the outcome function.
tournamentOutcome :: Tournament -> Integer
tournamentOutcome (m, m') = sum (zipWith outcome m m')

-- Type that represents a function that determines a behaviour for a future
-- move. It reads a series of opponent moves and 
type Strategy  = [Move] -> Move

-- Strategy that returns the move that would beat the opponents last move.
-- (Chooses Rock if there haven't been any moves yet)
sWinLast :: Strategy
sWinLast []    = Rock
sWinLast (m:_) = beat m

-- Strategy that returns the move that would beat the most moves 
-- made by the opponent. (Chooses Rock if there haven't been any moves yet)
mostFreqMove :: Strategy
mostFreqMove [] = Rock
mostFreqMove l  = beat (mostFreq l (0,0,0))
  where
    mostFreq [] (r,p,s) 
      | r >= p && r >= s = Rock
      | p >= r && p >= s = Paper
      | otherwise        = Scissors
    mostFreq (m:ms) (r,p,s)
      | m == Rock     = mostFreq ms (r+1,p,s)
      | m == Paper    = mostFreq ms (r,p+1,s)
      | m == Scissors = mostFreq ms (r,p,s+1)

-- Given 2 strategies, returns a new strategy that uses both alternately.
-- The alternation is based on the length of the list of moves.
alternate :: Strategy -> Strategy -> Strategy
alternate f1 f2 = newStrategy
  where
    newStrategy :: Strategy
    newStrategy l
      | (length l) `mod` 2 == 1 = f1 l
      | otherwise               = f2 l

--------------------------------------------------------------------------------

-- Given a strategy, runs an interactive game against the computer.
play :: Strategy -> IO ()
play strategy = playInteractive strategy ([], [])

-- Main function for the interactive game. Saves and shows on the screen how
-- the game is going. The moves are specified according to the first letter
-- of a message. If the first letter of the message is r, p or s 
-- (case insensitive), then the player will be playing rock, paper or scissors
-- as his move. If any other letter is used the tournament stops and the winner
-- is chosen.
playInteractive :: Strategy -> Tournament -> IO ()
playInteractive s t@(mine, yours) =
  do
    putStr "\nYour move is: "
    (ch:_) <- getLine
    if not (ch `elem` "rpsRPS")
      then showResults t
      else do let next = s yours
              let yourMove = convertMove ch
              putStrLn ("\nI play: "  ++
                        show next     ++ 
                        " you play: " ++ 
                        show yourMove
                       )
              playInteractive s (next:mine, yourMove:yours)

-- Shows the result of a tournament as a message.
showResults :: Tournament -> IO ()
showResults t 
  | tournamentOutcome t < 0 = putStrLn "You win: Well done!"
  | tournamentOutcome t > 0 = putStrLn "I win: Try again!"
  | otherwise               = putStrLn "Draw!"

-- Converts the first letter into the corresponding move
convertMove :: Char -> Move
convertMove ch
  | ch `elem` "rR" = Rock
  | ch `elem` "pP" = Paper
  | ch `elem` "sS" = Scissors
  | otherwise      = error "Not a valid move."

-- Explains the UI and lets the player choose an strategy
game :: IO ()
game = do
  putStrLn "Welcome to the Rock-Paper-Scissors Tournament"
  putStrLn "Enter r (or R) for Rock, p (or P) for Paper, s (or S) for Scissors"
  putStrLn "Enter anything else to quit!"
  putStrLn "Enjoy the game!\n"
  putStrLn "Choose a strategy to play against:"
  putStrLn "a) Greedy."
  putStrLn "b) Analyzer."
  putStrLn "c) Alternate the above two strategies."
  (ch:_) <- getLine
  if not (ch `elem` "abcABC")
    then putStrLn "Goodbye"
    else startGame ch
  where
    startGame ch
      | ch `elem` "aA" = play sWinLast
      | ch `elem` "bB" = play mostFreqMove
      | ch `elem` "cC" = play (alternate sWinLast mostFreqMove)

--------------------------------------------------------------------------------
-- Test Cases and Main Game
--------------------------------------------------------------------------------

tBeatAndLose :: String
tBeatAndLose = let
    test     =  (beat Rock) == Paper && (beat Paper) == Scissors && (beat Scissors) == Rock
    message  = "(beat Rock) == Paper && (beat Paper) == Scissors && (beat Scissors) == Rock"
    test2    =  (lose Rock) == Scissors && (lose Paper) == Rock && (lose Scissors) == Paper
    message2 = "(lose Rock) == Scissors && (lose Paper) == Rock && (lose Scissors) == Paper"  
  in
    message ++ " is: " ++ (show test) ++ "\n" ++
    message2 ++ " is: " ++ (show test2) 


toutcome :: String
toutcome = let
    test     =  outcome Rock Paper == -1
    message  = "outcome Rock Paper == Lost"
    test2    =  outcome Rock Rock == 0
    message2 = "outcome Rock Rock == Draw"
    test3    =  outcome Rock Scissors == 1
    message3 = "outcome Rock Scissors == Win"
  in
    message ++ " is: " ++ (show test) ++ "\n" ++
    message2 ++ " is: " ++ (show test2) ++ "\n" ++
    message3 ++ " is: " ++ (show test3) 

ttournament :: String
ttournament = let
    test     =  tournamentOutcome ([Rock,Paper,Paper], [Paper,Rock,Scissors]) < 0
    message  = "Outcome of [Rock,Paper,Paper] vs. [Paper,Rock,Scissors]) == Lost"
  in
    message ++ " is: " ++ (show test)


tstrategyWinLast :: String
tstrategyWinLast = let
    test     =  sWinLast [Paper, Rock, Rock] == Scissors
    message  = "Greedy Strategy against [Paper, Rock, Rock] == Scissors"
  in
    message ++ " is: " ++ (show test)


tstrategyFreq :: String
tstrategyFreq = let
    test     =  mostFreqMove [Paper ,Rock, Rock] == Paper
    message  = "Frequency Strategy against [Paper, Rock, Rock] == Paper"
  in
    message ++ " is: " ++ (show test)


tstrategyAlt :: String
tstrategyAlt = let
    test      =  (alternate sWinLast mostFreqMove) [Paper, Rock, Rock] == Scissors
    message   = "Alternate Strategy against [Paper, Rock, Rock] == Scissors"
    test2     =  (alternate sWinLast mostFreqMove) [Paper, Rock, Rock, Scissors] == Paper
    message2  = "Alternate Strategy against [Paper, Rock, Rock, Scissors] == Paper"
  in
    message ++ " is: " ++ (show test) ++ "\n" ++
    message2 ++ " is: " ++ (show test2) ++ "\n" 

tests :: [String]
tests = [ tBeatAndLose
        , toutcome
        , ttournament
        , tstrategyWinLast
        , tstrategyFreq
        , tstrategyAlt
        ]

main :: IO ()
main = do
  putStrLn "Test cases for the Rock-Paper-Scissors game:\n" 
  putStr  $ unlines $ map (++ "\n") tests
  
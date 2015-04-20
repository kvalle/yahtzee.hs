module Yahtzee.Game where

import Text.Printf
import System.IO (hFlush, stdout)
import System.Random (newStdGen)
import Data.Tuple.Select

import Yahtzee.Hand
import Yahtzee.ScoreCard


-- Types

data Try = FirstTry | SecondTry | FinalTry deriving (Eq, Ord, Enum)

instance Show Try where
    show FirstTry  = "1st"
    show SecondTry = "2nd"
    show FinalTry  = "Final"

-- Game interactions

play :: IO ()
play = do
    card <- playRounds 13 emptyScoreCard
    putStrLn "\n\nGAME OVER"

playRounds :: Int -> ScoreCard -> IO ScoreCard
playRounds 0 card = return card
playRounds n card = do
    hand <- playRoll FirstTry EmptyHand
    card <- scoreHand hand card
    playRounds (n - 1) card

playRoll :: Try -> Hand -> IO Hand
playRoll _ hand | allHeld hand = return hand
playRoll try hand = do
    gen <- newStdGen
    let newHand = rerollHand gen hand
    putStrLn $ printf "\n%s roll \n===================" $ show try
    if try == FinalTry
        then do
            putStrLn $ show newHand
            return newHand
        else do
            newHand' <- holdDices newHand
            playRoll (succ try) newHand'

holdDices :: Hand -> IO Hand
holdDices hand = do
    putStr $ (++) (show hand) "  Hold? "
    hFlush stdout
    n <- getChar
    putStrLn ""
    case n of
        n | n `elem` "12345"
             -> holdDices $ holdOneToggle (read [n] - 1) hand
        '\n' -> return hand
        'a'  -> holdDices $ holdAll hand
        _    -> do
            putStrLn "> Use the numbers 1 - 5 to select dice, or ENTER to continue."
            holdDices hand

scoreHand :: Hand -> ScoreCard -> IO ScoreCard
scoreHand hand card = do
    printScore hand card
    card <- scoreCategory hand card
    return card

scoreCategory :: Hand -> ScoreCard -> IO ScoreCard
scoreCategory (Hand hand) card = do
    putStr "Select category [a-m]: "
    hFlush stdout
    n <- getChar
    putStrLn ""
    case n of
        n    | n `elem` (map sel1 card) -> 
            case getCategory n card of
                (cid, cname, NoValue, scoreFn) -> 
                    let values = map fst hand
                        updated = (cid, cname, Score (scoreFn values), scoreFn)
                    in return $ updateCategory updated card
                (_, _, Score _, _) -> do
                    putStrLn "> Category already scored"
                    scoreCategory (Hand hand) card
        otherwise -> do
            putStrLn "> Invalid category!"
            scoreCategory (Hand hand) card

printScore :: Hand -> ScoreCard -> IO ()
printScore (Hand hand) card = do
    putStrLn "\n"
    mapM_ (\(cid, category, score, scoreFn) -> row cid category score (scoreFn (map fst hand))) card

    where
        row cid category (Score score) _   = putStrLn $ printf "%c) %-19s %2d" cid category score
        row cid category NoValue handScore = putStrLn $ printf "%c) %-19s %2d ?" cid category handScore


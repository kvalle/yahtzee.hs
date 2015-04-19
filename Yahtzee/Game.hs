module Yahtzee.Game where

import Text.Printf
import System.IO (hFlush, stdout)
import System.Random (newStdGen)

import Yahtzee.Scoring
import Yahtzee.Hand

-- Types

data Try = FirstTry | SecondTry | FinalTry deriving (Eq, Ord, Enum)

instance Show Try where
    show FirstTry  = "1st"
    show SecondTry = "2nd"
    show FinalTry  = "Final"

-- Game interactions

play :: IO ()
play = do
    card <- playRounds 3 newScoreCard
    putStrLn $ show card

playRounds :: Int -> ScoreCard -> IO ScoreCard
playRounds 0 card = return card
playRounds n card = do
    hand <- playRoll FirstTry EmptyHand
    card <- score hand card
    playRounds (n - 1) card

playRoll :: Try -> Hand -> IO Hand
playRoll _ hand | allHeld hand = return hand
playRoll try hand = do
    gen <- newStdGen
    let newHand = rerollHand gen hand
    putStrLn $ (show try) ++ " roll \n==================="
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

score :: Hand -> ScoreCard -> IO ScoreCard
score hand card = do
    printHand hand
    -- todo: update scorecard
    return card

printHand :: Hand -> IO ()
printHand (Hand hand) = do
    let values = map fst hand
    putStrLn "\nCategory\n========"
    putStrLn $ printf "Ones:            %3d" $ scoreN 1 values
    putStrLn $ printf "Twos:            %3d" $ scoreN 2 values
    putStrLn $ printf "Threes:          %3d" $ scoreN 3 values
    putStrLn $ printf "Fours:           %3d" $ scoreN 4 values
    putStrLn $ printf "Fives:           %3d" $ scoreN 5 values
    putStrLn $ printf "Sixes:           %3d" $ scoreN 6 values
    putStrLn $ printf "Yahtzee:         %3d" $ scoreYahtzee values
    putStrLn $ printf "Small straight:  %3d" $ scoreSmallStraight values
    putStrLn $ printf "Large straight:  %3d" $ scoreLargeStraight values
    putStrLn $ printf "Three of a kind: %3d" $ scoreThreeOfAKind values
    putStrLn $ printf "Four of a kind:  %3d" $ scoreFourOfAKind values
    putStrLn $ printf "Full house:      %3d" $ scoreFullHouse values
    putStrLn $ printf "Chance:          %3d" $ scoreChance values
    putStrLn ""

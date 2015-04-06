module Yahtzee.Game where

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
    evaluate hand
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

evaluate :: Hand -> IO ()
evaluate (Hand hand) = do
    let values = map fst hand
    putStrLn "\nResults\n========"
    putStrLn $ (++) "Ones:            " $ show $ scoreN 1 values
    putStrLn $ (++) "Twos:            " $ show $ scoreN 2 values
    putStrLn $ (++) "Threes:          " $ show $ scoreN 3 values
    putStrLn $ (++) "Fours:           " $ show $ scoreN 4 values
    putStrLn $ (++) "Fives:           " $ show $ scoreN 5 values
    putStrLn $ (++) "Sixes:           " $ show $ scoreN 6 values
    putStrLn $ (++) "Yahtzee:         " $ show $ scoreYahtzee values
    putStrLn $ (++) "Small straight:  " $ show $ scoreSmallStraight values
    putStrLn $ (++) "Large straight:  " $ show $ scoreLargeStraight values
    putStrLn $ (++) "Three of a kind: " $ show $ scoreThreeOfAKind values
    putStrLn $ (++) "Four of a kind:  " $ show $ scoreFourOfAKind values
    putStrLn $ (++) "Full house:      " $ show $ scoreFullHouse values
    putStrLn $ (++) "Chance:          " $ show $ scoreChance values
    putStrLn ""

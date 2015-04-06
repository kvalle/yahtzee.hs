module Yahtzee.Game where

import System.IO (hFlush, stdout)
import System.Random (newStdGen)

import Yahtzee.Scoring (evaluate)
import Yahtzee.Hand

play :: IO ()
play = do
    hand <- playRoll FirstTry EmptyHand
    evaluate hand

-- Types

data Try = FirstTry | SecondTry | FinalTry deriving (Eq, Ord, Enum)

instance Show Try where
    show FirstTry = "1st"
    show SecondTry = "2nd"
    show FinalTry = "Final"

-- Game interactions

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

module Yahtzee.Game where

import System.IO
import System.Random

import Control.Applicative

import Data.Char (toUpper)
import Data.List
import Data.Sequence (fromList, update)
import Data.Foldable (toList)

import Yahtzee.Scoring (evaluate)
import Yahtzee.Types


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
playRoll _ hand | allKept hand = return hand
playRoll try hand = do
    gen <- newStdGen
    let newHand = reroll gen hand
    printRollNumber try
    if try == FinalTry
        then do
            putStrLn $ show newHand
            return newHand
        else do
            newHand' <- keepDices newHand
            playRoll (succ try) newHand'

keepDices :: Hand -> IO Hand
keepDices hand = do
    putStr $ (++) (show hand) "  Keep? "
    hFlush stdout
    n <- getChar
    putStrLn ""
    case n of
        n | n `elem` "12345"
             -> keepDices $ toogleKeep (read [n] - 1) hand
        '\n' -> return hand
        'a'  -> keepDices hand  -- todo: allKept
        _    -> do
            putStrLn "> Use the numbers 1 - 5 to select dice, or ENTER to continue."
            keepDices hand

printRollNumber :: Try -> IO ()
printRollNumber roll = putStrLn $ (++) (show roll) " roll \n==================="


-- Functions to be moved out of this module

toogleKeep :: Int -> Hand -> Hand
toogleKeep n (Hand hand) = Hand $ replace n toogled hand
        where 
            (val, keep) = hand !! n
            toogled = (val, not keep)
            replace i val list = toList $ update i val $ fromList list

allKept :: Hand -> Bool
allKept (Hand hand) = (and . map snd) hand
allKept EmptyHand = False

roll :: (RandomGen g) => g -> [Int]
roll gen = take 5 $ randomRs (1,6) gen

reroll :: (RandomGen g) => g -> Hand -> Hand
reroll gen EmptyHand = Hand $ zip (roll gen) $ replicate 5 False
reroll gen (Hand hand) =
    Hand $ zip (zipWith3 (\o n k -> if k then o else n) old new keep) keep
        where new = roll gen
              old = map fst hand
              keep = map snd hand

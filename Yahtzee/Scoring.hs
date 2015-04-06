module Yahtzee.Scoring (evaluate) where

import Data.List
import Yahtzee.Types

evaluate :: Hand -> IO ()
evaluate (Hand hand) = do
    let values = map fst hand
    putStrLn "\nResults\n========"
    putStrLn $ (++) "Yahtzee:         " $ show $ isYahtzee values
    putStrLn $ (++) "Small straight:  " $ show $ isSmallStraight values
    putStrLn $ (++) "Large straight:  " $ show $ isLargeStraight values
    putStrLn $ (++) "Three of a kind: " $ show $ isThreeOfAKind values
    putStrLn $ (++) "Four of a kind:  " $ show $ isFourOfAKind values
    putStrLn $ (++) "Full house:      " $ show $ isFullHouse values

isYahtzee :: [Int] -> Bool
isYahtzee (x:rest) = all (== x) rest

isSmallStraight :: [Int] -> Bool
isSmallStraight roll = or [[1,2,3,4] `isInfixOf` r, 
                           [2,3,4,5] `isInfixOf` r,
                           [3,4,5,6] `isInfixOf` r]
    where r = (map head . group . sort) roll

isLargeStraight :: [Int] -> Bool
isLargeStraight roll = or [[1,2,3,4,5] `isInfixOf` r, 
                           [2,3,4,5,6] `isInfixOf` r]
    where r = (map head . group . sort) roll

isThreeOfAKind :: [Int] -> Bool
isThreeOfAKind = hasNAlike 3

isFourOfAKind :: [Int] -> Bool
isFourOfAKind = hasNAlike 4

hasNAlike :: Int -> [Int] -> Bool
hasNAlike n = elem n . map length . group . sort

isFullHouse :: [Int] -> Bool
isFullHouse roll = [2,3] == (sort . map length . group . sort) roll

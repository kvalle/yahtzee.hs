module Yahtzee.Scoring where

import Data.List


isYahtzee :: [Int] -> Bool
isYahtzee (x:rest) = all (== x) rest

isSmallStraight :: [Int] -> Bool
isSmallStraight roll = or [[1,2,3,4] `isInfixOf` roll, 
                           [2,3,4,5] `isInfixOf` roll,
                           [3,4,5,6] `isInfixOf` roll]
    where r = sort roll

isLargeStraight :: [Int] -> Bool
isLargeStraight roll = or [[1,2,3,4,5] `isInfixOf` r, 
                           [2,3,4,5,6] `isInfixOf` r]
    where r = sort roll

isThreeOfAKind :: [Int] -> Bool
isThreeOfAKind = hasNAlike 3

isFourOfAKind :: [Int] -> Bool
isFourOfAKind = hasNAlike 4

hasNAlike :: Int -> [Int] -> Bool
hasNAlike n = elem n . map length . group . sort

isFullHouse :: [Int] -> Bool
isFullHouse roll = [2,3] == (sort . map length . group . sort) roll


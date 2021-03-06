module Yahtzee.Scoring where

import Text.Show.Functions
import Data.Tuple.Select
import Data.List

-- Upper section

scoreN :: Int -> [Int] -> Int
scoreN n = sum . filter (==n)


-- Straights

isSmallStraight :: [Int] -> Bool
isSmallStraight roll = or [[1,2,3,4] `isInfixOf` r, 
                           [2,3,4,5] `isInfixOf` r,
                           [3,4,5,6] `isInfixOf` r]
    where r = (map head . group . sort) roll

isLargeStraight :: [Int] -> Bool
isLargeStraight roll = or [[1,2,3,4,5] `isInfixOf` r, 
                           [2,3,4,5,6] `isInfixOf` r]
    where r = (map head . group . sort) roll

scoreSmallStraight :: [Int] -> Int
scoreSmallStraight vals = if isSmallStraight vals then 30 else 0

scoreLargeStraight :: [Int] -> Int
scoreLargeStraight vals = if isLargeStraight vals then 40 else 0


-- N of a kind

isThreeOfAKind :: [Int] -> Bool
isThreeOfAKind = hasNAlike 3

isFourOfAKind :: [Int] -> Bool
isFourOfAKind = hasNAlike 4

scoreThreeOfAKind :: [Int] -> Int
scoreThreeOfAKind vals = if isThreeOfAKind vals then sum vals else 0

scoreFourOfAKind :: [Int] -> Int
scoreFourOfAKind vals = if isFourOfAKind vals then sum vals else 0

hasNAlike :: Int -> [Int] -> Bool
hasNAlike n = any (>=n) . map length . group . sort


-- House

isFullHouse :: [Int] -> Bool
isFullHouse roll = [2,3] == (sort . map length . group . sort) roll

scoreFullHouse :: [Int] -> Int
scoreFullHouse vals = if isFullHouse vals then 25 else 0


-- Chance

scoreChance :: [Int] -> Int
scoreChance = sum


-- Yahtzee

isYahtzee :: [Int] -> Bool
isYahtzee (x:rest) = all (== x) rest

scoreYahtzee :: [Int] -> Int
scoreYahtzee vals = if isYahtzee vals then 50 else 0

module Yahtzee.Scoring where

import Data.List

data Score = Score Int | NoValue deriving (Show)

data ScoreCard = ScoreCard { ones :: Score
                           , twos :: Score
                           , threes :: Score
                           , fours :: Score
                           , fives :: Score
                           , sixes :: Score
                           , smallStraight :: Score
                           , largeStraight :: Score
                           , threeOfAKind :: Score
                           , fourOfAKind :: Score
                           , fullHouse :: Score
                           , yahtzee :: Score
                           , chance :: Score
                           } deriving (Show)

emptyScoreCard = ScoreCard 
    NoValue NoValue NoValue NoValue NoValue 
    NoValue NoValue NoValue NoValue NoValue 
    NoValue NoValue NoValue

readCard :: Char -> ScoreCard -> Score
readCard 'a' = ones
readCard 'b' = twos
readCard 'c' = threes
readCard 'd' = fours
readCard 'e' = fives
readCard 'f' = sixes
readCard 'g' = smallStraight
readCard 'h' = largeStraight
readCard 'i' = threeOfAKind
readCard 'j' = fourOfAKind
readCard 'k' = fullHouse
readCard 'l' = yahtzee
readCard 'm' = chance

updateCard :: Char -> ScoreCard -> Score -> ScoreCard
updateCard 'a' card value = card {ones = value}
updateCard 'b' card value = card {twos = value}
updateCard 'c' card value = card {threes = value}
updateCard 'd' card value = card {fours = value}
updateCard 'e' card value = card {fives = value}
updateCard 'f' card value = card {sixes = value}
updateCard 'g' card value = card {smallStraight = value}
updateCard 'h' card value = card {largeStraight = value}
updateCard 'i' card value = card {threeOfAKind = value}
updateCard 'j' card value = card {fourOfAKind = value}
updateCard 'k' card value = card {fullHouse = value}
updateCard 'l' card value = card {yahtzee = value}
updateCard 'm' card value = card {chance = value}

score :: Char -> [Int] -> Int
score 'a' = scoreN 1
score 'b' = scoreN 2
score 'c' = scoreN 3
score 'd' = scoreN 4
score 'e' = scoreN 5
score 'f' = scoreN 6
score 'g' = scoreSmallStraight
score 'h' = scoreLargeStraight
score 'i' = scoreThreeOfAKind
score 'j' = scoreFourOfAKind
score 'k' = scoreFullHouse
score 'l' = scoreYahtzee
score 'm' = scoreChance

-- Upper section

scoreN :: Int -> [Int] -> Int
scoreN n = sum . filter (==n)

-- Yahtzee

isYahtzee :: [Int] -> Bool
isYahtzee (x:rest) = all (== x) rest

scoreYahtzee :: [Int] -> Int
scoreYahtzee vals = if isYahtzee vals then 50 else 0


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
hasNAlike n = elem n . map length . group . sort


-- House

isFullHouse :: [Int] -> Bool
isFullHouse roll = [2,3] == (sort . map length . group . sort) roll

scoreFullHouse :: [Int] -> Int
scoreFullHouse vals = if isFullHouse vals then 25 else 0


-- Chance

scoreChance :: [Int] -> Int
scoreChance = sum

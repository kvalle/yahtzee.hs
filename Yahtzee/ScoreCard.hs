module Yahtzee.ScoreCard where

import Text.Printf
import Text.Show.Functions
import Data.Tuple.Select

import Yahtzee.Scoring

data Score = Score Int | NoValue

instance Show Score where
    show NoValue   = "  -"
    show (Score n) = printf "%3d" n

type CategoryName = String
type CategoryId = Char
type ScoreFn = [Int] -> Int
type Category = (CategoryId, CategoryName, Score, ScoreFn)
type ScoreCard = [Category]

total :: [Score] -> Int
total [] = 0
total ((Score n):rest) = n + (total rest)
total (NoValue:rest) = total rest

getCategory :: CategoryId -> ScoreCard -> Category
getCategory cid card = head $ dropWhile wrong card
    where wrong cat = (/=) cid $ sel1 cat

updateCategory :: Category -> ScoreCard -> ScoreCard
updateCategory new card = takeWhile wrong card ++ [new] ++ tail (dropWhile wrong card)
    where cid = sel1 new
          wrong cat = (/=) cid $ sel1 cat

emptyScoreCard = [
    ('a', "Ones", NoValue, scoreN 1),
    ('b', "Twos", NoValue, scoreN 2),
    ('c', "Threes", NoValue, scoreN 3),
    ('d', "Fours", NoValue, scoreN 4),
    ('e', "Fives", NoValue, scoreN 5),
    ('f', "Sixes", NoValue, scoreN 6),
    ('g', "Small straight", NoValue, scoreSmallStraight),
    ('h', "Large straight", NoValue, scoreLargeStraight),
    ('i', "Three of a kind", NoValue, scoreThreeOfAKind),
    ('j', "Four of a kind", NoValue, scoreFourOfAKind),
    ('k', "Full house", NoValue, scoreFullHouse),
    ('l', "Yahtzee", NoValue, scoreYahtzee),
    ('m', "Chance", NoValue, scoreChance)]

module Yahtzee.Hand where

import System.Random (RandomGen, randomRs, mkStdGen)
import Data.List (intercalate)
import Data.Sequence (fromList, update)
import Data.Foldable (toList)

data Hand = Hand [(Int, Bool)] | EmptyHand deriving (Show)

reroll :: (RandomGen g) => g -> Hand -> Hand
reroll gen EmptyHand = Hand $ zip (roll gen) $ replicate 5 False
reroll gen (Hand hand) = 
    Hand $ zipWith (\ (o, h) n -> if h then (o, h) else (n, h)) hand $ roll gen

roll :: (RandomGen g) => g -> [Int]
roll gen = take 5 $ randomRs (1,6) gen

hold :: Int -> Hand -> Hand
hold n (Hand hand) = Hand $ replace n toogled hand
        where 
            (val, hold) = hand !! n
            toogled = (val, not hold)
            replace i val list = toList $ update i val $ fromList list

holdAll :: Hand -> Hand
holdAll (Hand hand) = Hand $ zip values $ replicate 5 True
    where values = map fst hand

allHeld :: Hand -> Bool
allHeld (Hand hand) = (and . map snd) hand
allHeld EmptyHand = False

formatHand :: Hand -> String
formatHand EmptyHand = " -   -   -   -   - "
formatHand (Hand h) = intercalate " " $ map formatDie h
    where formatDie (i, k) = if k then "(" ++ (show i) ++ ")"
                                  else " " ++ (show i) ++ " "

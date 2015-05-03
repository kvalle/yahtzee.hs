module Yahtzee.Hand where

import System.Random (RandomGen, randomRs, mkStdGen)
import Data.List (intercalate)
import Data.Sequence (fromList, update)
import Data.Foldable (toList)

data Hand = Hand [(Int, Bool)] | EmptyHand deriving (Show)

-- | Creates a new Hand from list of dice values
--
-- >>> newHand [1..3]
-- Hand [(1,False),(2,False),(3,False)]
newHand :: [Int] -> Hand
newHand values = Hand $ zip values $ replicate 5 False

-- | Returns a list of five dice (ints) given a random generator.
--
-- >>> roll (mkStdGen 1)
-- [6,5,2,6,5]
roll :: (RandomGen g) => g -> [Int]
roll gen = take 5 $ randomRs (1,6) gen

rerollHand :: (RandomGen g) => g -> Hand -> Hand
rerollHand gen EmptyHand = newHand $ roll gen
rerollHand gen (Hand hand) = 
    Hand $ zipWith (\ (o, h) n -> if h then (o, h) else (n, h)) hand $ roll gen

printHand :: Hand -> String
printHand EmptyHand = " -   -   -   -   - "
printHand (Hand h) = intercalate " " $ map formatDie h
    where formatDie (i, k) = if k then "(" ++ (show i) ++ ")"
                                  else " " ++ (show i) ++ " "

holdOneToggle :: Int -> Hand -> Hand
holdOneToggle n (Hand hand) = Hand $ replace n toogled hand
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

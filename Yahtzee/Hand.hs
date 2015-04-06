module Yahtzee.Hand where

import System.Random (RandomGen, randomRs)
import Data.List (intercalate)
import Data.Sequence (fromList, update)
import Data.Foldable (toList)

data Hand = Hand [(Int, Bool)] | EmptyHand

instance Show Hand where
    show EmptyHand = " -   -   -   -   - "
    show (Hand h) = intercalate " " $ map formatDie h
        where formatDie (i, k) = if k then "(" ++ (show i) ++ ")"
                                      else " " ++ (show i) ++ " "

newHand :: [Int] -> Hand
newHand values = Hand $ zip values $ replicate 5 False

-- Rolling dice

roll :: (RandomGen g) => g -> [Int]
roll gen = take 5 $ randomRs (1,6) gen

reroll :: (RandomGen g) => g -> Hand -> Hand
reroll gen EmptyHand = newHand $ roll gen
reroll gen (Hand hand) =
    Hand $ zip (zipWith3 (\o n k -> if k then o else n) old new keep) keep
        where new = roll gen
              old = map fst hand
              keep = map snd hand

-- Keeping dices

keepOneToggle :: Int -> Hand -> Hand
keepOneToggle n (Hand hand) = Hand $ replace n toogled hand
        where 
            (val, keep) = hand !! n
            toogled = (val, not keep)
            replace i val list = toList $ update i val $ fromList list

keepAll :: Hand -> Hand
keepAll (Hand hand) = Hand $ zip values $ replicate 5 True
	where values = map fst hand

allKept :: Hand -> Bool
allKept (Hand hand) = (and . map snd) hand
allKept EmptyHand = False

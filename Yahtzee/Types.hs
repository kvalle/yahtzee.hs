module Yahtzee.Types where

import Data.List

data Hand = Hand [(Int, Bool)] | EmptyHand

instance Show Hand where
    show EmptyHand = " -   -   -   -   - "
    show (Hand h) = intercalate " " $ map formatDie h
        where formatDie (i, k) = if k then "(" ++ (show i) ++ ")"
                                      else " " ++ (show i) ++ " "

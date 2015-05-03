module Main where

import Test.DocTest

main :: IO ()
main = doctest ["src/Yahtzee/Hand.hs"]

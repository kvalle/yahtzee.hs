
import Data.List
import System.Random  
  
main = do  
    gen <- getStdGen
    let roll = rollDice gen

    putStrLn $ formatRoll roll
    putStrLn $ (++) "Yahtzee:         " $ show $ isYahtzee roll
    putStrLn $ (++) "Small straight:  " $ show $ isSmallStraight roll
    putStrLn $ (++) "Large straight:  " $ show $ isLargeStraight roll
    putStrLn $ (++) "Three of a kind: " $ show $ isThreeOfAKind roll
    putStrLn $ (++) "Four of a kind:  " $ show $ isFourOfAKind roll
    putStrLn $ (++) "Full house:      " $ show $ isFullHouse roll

rollDice :: (RandomGen g) => g -> [Int]  
rollDice gen = sort $ take 5 $ randomRs (1,6) gen

formatKeeps :: [Bool] -> String
formatKeeps keeps = intercalate " " $ map (\k -> "[" ++ if k then "x" else " " ++ "]") keeps

formatRoll :: [Int] -> String
formatRoll roll = " " ++ (intercalate "   " $ map show roll)


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


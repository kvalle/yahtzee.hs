import System.IO
import System.Random

import Control.Applicative

import Data.List
import Data.Sequence (fromList, update)
import Data.Foldable (toList)

emptyKeep = replicate 5 False
nilRoll = replicate 5 0


main = do
    hSetBuffering stdin NoBuffering
    finalRoll <- play 1 nilRoll $ replicate 5 False
    printResult finalRoll

play :: Int -> [Int] -> [Bool] -> IO [Int]
play n dices keeps  | n > 3 = reroll dices keeps
play _ dices keeps | and keeps = return dices
play n dices keeps = do
    putStrLn $ "ROLL " ++ (show n) ++ "\n======"
    newRoll <- reroll dices keeps
    newKeeps <- getKeeps newRoll keeps
    play (n + 1) newRoll newKeeps

reroll :: [Int] -> [Bool] -> IO [Int]
reroll oldRoll keeps = do
    newRoll <- rollDice <$> newStdGen
    return $ zipWith3 (\o n k -> if k then o else n) oldRoll newRoll keeps

getKeeps :: [Int] -> [Bool] -> IO [Bool]
getKeeps roll keeps = do
    putStr $ (formatKeeps roll keeps) ++ "  Keep? "
    hFlush stdout
    n <- getChar
    putStrLn ""
    case n of
        n | n `elem` "12345" -> getKeeps roll $ toogleKeeps (read [n] - 1) keeps
        '\n' -> return keeps
        'a' -> getKeeps roll $ replicate 5 True
        _    -> do
            putStrLn "> Use the numbers 1 - 5 to keep dice, ENTER to continue."
            getKeeps roll keeps

toogleKeeps :: Int -> [Bool] -> [Bool]
toogleKeeps n keeps = replace n newValue keeps
    where newValue = not $ keeps !! n
          replace i val list = toList $ update i val $ fromList list

formatKeeps :: [Int] -> [Bool] -> String
formatKeeps roll keeps = intercalate " " $ zipWith zipFn keeps $ show <$> roll
    where zipFn = (\ k i -> if k then "(" ++ i ++ ")" else " " ++ i ++ " ")


-- Dice

rollDice :: (RandomGen g) => g -> [Int]
rollDice gen = take 5 $ randomRs (1,6) gen

printResult :: [Int] -> IO ()
printResult roll = do
    putStrLn $ (++) "Yahtzee:         " $ show $ isYahtzee roll
    putStrLn $ (++) "Small straight:  " $ show $ isSmallStraight roll
    putStrLn $ (++) "Large straight:  " $ show $ isLargeStraight roll
    putStrLn $ (++) "Three of a kind: " $ show $ isThreeOfAKind roll
    putStrLn $ (++) "Four of a kind:  " $ show $ isFourOfAKind roll
    putStrLn $ (++) "Full house:      " $ show $ isFullHouse roll

formatRoll :: [Int] -> String
formatRoll roll = " " ++ (intercalate "   " $ map show roll)


-- Check roll

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


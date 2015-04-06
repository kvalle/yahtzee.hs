import System.IO
import System.Random

import Control.Applicative

import Data.List
import Data.Sequence (fromList, update)
import Data.Foldable (toList)

import Yahtzee.Scoring

main = do
    hSetBuffering stdin NoBuffering
    finalRoll <- play 1 (replicate 5 0) (replicate 5 False)
    putStrLn $ "FINAL ROLL\n=========="
    putStrLn $ formatRoll finalRoll
    putStrLn ""
    printResult finalRoll

play :: Int -> [Int] -> [Bool] -> IO [Int]
play n dices keeps  | n == 3 = reroll dices keeps
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


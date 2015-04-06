import System.IO
import System.Random

import Control.Applicative

import Data.Char (toUpper)
import Data.List
import Data.Sequence (fromList, update)
import Data.Foldable (toList)

import Yahtzee.Scoring (evaluate)

data Roll = FirstRoll | SecondRoll | FinalRoll deriving (Eq, Ord, Enum)

instance Show Roll where
    show FirstRoll = "1st"
    show SecondRoll = "2nd"
    show FinalRoll = "Final"

type Dices = [Int]
type Keeps = [Bool]

keepNone = replicate 5 False
keepAll  = replicate 5 True


main = do
    randGen <- newStdGen
    hSetBuffering stdin NoBuffering

    dices <- playRoll FirstRoll (replicate 5 0) keepNone
    evaluate dices

playRoll :: Roll -> Dices -> Keeps -> IO Dices
playRoll _ dices keeps | and keeps = return dices
playRoll roll dices keeps = do
    newDices <- reroll dices keeps
    putStrLn $ (++) (show roll) " roll \n==================="
    if roll == FinalRoll
        then do
            putStrLn $ (++) " " $ intercalate "   " $ map show newDices
            return newDices
        else do
            newKeeps <- getKeeps newDices keeps
            playRoll (succ roll) newDices newKeeps
    
reroll :: Dices -> Keeps -> IO Dices
reroll oldDices keeps = do
    newDices <- rollDices <$> newStdGen
    return $ zipWith3 (\o n k -> if k then o else n) oldDices newDices keeps

getKeeps :: Dices -> Keeps -> IO Keeps
getKeeps dices keeps = do
    putStr $ (formatKeeps dices keeps) ++ "  Keep? "
    hFlush stdout
    n <- getChar
    putStrLn ""
    case n of
        n | n `elem` "12345" -> getKeeps dices $ toogleKeeps (read [n] - 1) keeps
        '\n' -> return keeps
        'a' -> getKeeps dices $ replicate 5 True
        _    -> do
            putStrLn "> Use the numbers 1 - 5 to select dice, or ENTER to continue."
            getKeeps dices keeps

toogleKeeps :: Int -> Keeps -> Keeps
toogleKeeps n keeps = replace n newValue keeps
    where newValue = not $ keeps !! n
          replace i val list = toList $ update i val $ fromList list

formatKeeps :: Dices -> Keeps -> String
formatKeeps dices keeps = intercalate " " $ zipWith zipFn keeps $ show <$> dices
    where zipFn = (\ k i -> if k then "(" ++ i ++ ")" else " " ++ i ++ " ")

rollDices :: (RandomGen g) => g -> Dices
rollDices gen = take 5 $ randomRs (1,6) gen

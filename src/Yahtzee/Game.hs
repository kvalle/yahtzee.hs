module Yahtzee.Game where

import Control.Monad
import Text.Printf
import System.IO (hFlush, stdout)
import System.Random (newStdGen)
import Data.Tuple.Select

import Yahtzee.Hand
import Yahtzee.ScoreCard

play :: IO ()
play = do
    card <- playRounds 1 emptyScoreCard
    putStrLn "\nGAME OVER"

data Try = FirstTry | SecondTry | FinalTry deriving (Eq, Ord, Enum)

instance Show Try where
    show FirstTry  = "1st"
    show SecondTry = "2nd"
    show FinalTry  = "Final"

playRounds :: Int -> ScoreCard -> IO ScoreCard
playRounds 14 card = return card
playRounds n card = do
    putStrLn $ printf "\nROUND %d\n" n
    when (n == 1) putKeyHelp
    hand <- playRoll FirstTry EmptyHand
    card <- scoreHand hand card
    playRounds (n + 1) card

putKeyHelp :: IO ()
putKeyHelp = do
    putStrLn "  > [1-5]: toggle dice holding"
    putStrLn "  > ENTER: go to next roll"
    putStrLn ""

playRoll :: Try -> Hand -> IO Hand
playRoll _ hand | allHeld hand = return hand
playRoll try hand = do
    gen <- newStdGen
    let newHand = rerollHand gen hand
    putStrLn $ printf "  %s roll \n  ===================" $ show try
    if try == FinalTry
        then do
            putStrLn $ printf "  %s" $ printHand newHand
            return newHand
        else do
            newHand' <- holdDices newHand
            playRoll (succ try) newHand'

holdDices :: Hand -> IO Hand
holdDices hand = do
    putStr $ printf "  %s  Hold? " $ printHand hand
    hFlush stdout
    n <- getChar
    putStrLn ""
    case n of
        n | n `elem` "12345"
             -> holdDices $ holdOneToggle (read [n] - 1) hand
        '\n' -> return hand
        'a'  -> holdDices $ holdAll hand
        _    -> do
            putStrLn "  > Use the numbers 1 - 5 to select dice, or ENTER to continue."
            holdDices hand

scoreHand :: Hand -> ScoreCard -> IO ScoreCard
scoreHand hand card = do
    printRollResult hand card
    card <- scoreCategory hand card
    printScoreCard card
    return card

printRollResult :: Hand -> ScoreCard -> IO ()
printRollResult (Hand hand) card = do
    putStrLn "\n  Scoring options:\n"
    mapM_ putCat card
    putStrLn ""

    where
        putCat (cid, cname, Score _, scoreFn) = return ()
        putCat (cid, cname, NoValue, scoreFn) = 
            putStrLn $ printf "  %c) %-17s %3d" cid cname (scoreFn (map fst hand))

scoreCategory :: Hand -> ScoreCard -> IO ScoreCard
scoreCategory (Hand hand) card = do
    putStr "  Select category [a-m]: "
    hFlush stdout
    n <- getChar
    case n of
        n | n `elem` (map sel1 card) -> 
            case getCategory n card of
                (cid, cname, NoValue, scoreFn) -> 
                    let values = map fst hand
                        updated = (cid, cname, Score (scoreFn values), scoreFn)
                    in do 
                        putStrLn ""
                        return $ updateCategory updated card
                (_, _, Score _, _) -> tryAgain "\n  > Category already scored"
        '\n'      -> tryAgain "  > Please choose a category"
        otherwise -> tryAgain "\n  > Invalid category"

    where 
        tryAgain msg = do
            putStrLn msg
            scoreCategory (Hand hand) card

printScoreCard :: ScoreCard -> IO ()
printScoreCard card = do
    putStrLn ""
    putLeft "SCORE CARD" ""
    emptyRow
    mapM_ putCat upper
    putRight "Sum" $ show sumUpper
    putRight "Bonus" $ show bonus
    emptyRow
    mapM_ putCat lower
    emptyRow
    putRight "Total" $ show (sumLower + sumUpper)
    putStrLn ""

    where
        upper = take 6 card
        lower = drop 6 card
        sumUpper = total $ map sel3 upper
        sumLower = total $ map sel3 lower
        bonus = if sumUpper <= 63 then 0 else 35
        putCat (_, category, score, _) = putLeft category $ show score
        putLeft name value = putStrLn $ printf "  [  %-17s %3s  ]" name value
        putRight name value = putStrLn $ printf "  [  %17s %3s  ]" name value
        emptyRow = putStrLn "  [                         ]"

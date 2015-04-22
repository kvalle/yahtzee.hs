module Yahtzee.Game where

import Text.Printf
import System.IO (hFlush, stdout)
import System.Random (newStdGen)
import Data.Tuple.Select

import Yahtzee.Hand
import Yahtzee.ScoreCard

-- Game interactions

play :: IO ()
play = do
    card <- playRounds 13 emptyScoreCard
    putStrLn "\n\nGAME OVER"


data Try = FirstTry | SecondTry | FinalTry deriving (Eq, Ord, Enum)

instance Show Try where
    show FirstTry  = "1st"
    show SecondTry = "2nd"
    show FinalTry  = "Final"

playRounds :: Int -> ScoreCard -> IO ScoreCard
playRounds 0 card = return card
playRounds n card = do
    hand <- playRoll FirstTry EmptyHand
    card <- scoreHand hand card
    playRounds (n - 1) card

playRoll :: Try -> Hand -> IO Hand
playRoll _ hand | allHeld hand = return hand
playRoll try hand = do
    gen <- newStdGen
    let newHand = rerollHand gen hand
    putStrLn $ printf "\n%s roll \n===================" $ show try
    if try == FinalTry
        then do
            putStrLn $ show newHand
            return newHand
        else do
            newHand' <- holdDices newHand
            playRoll (succ try) newHand'

holdDices :: Hand -> IO Hand
holdDices hand = do
    putStr $ (++) (show hand) "  Hold? "
    hFlush stdout
    n <- getChar
    putStrLn ""
    case n of
        n | n `elem` "12345"
             -> holdDices $ holdOneToggle (read [n] - 1) hand
        '\n' -> return hand
        'a'  -> holdDices $ holdAll hand
        _    -> do
            putStrLn "> Use the numbers 1 - 5 to select dice, or ENTER to continue."
            holdDices hand

scoreHand :: Hand -> ScoreCard -> IO ScoreCard
scoreHand hand card = do
    printRollResult hand card
    card <- scoreCategory hand card
    printScoreCard card
    return card

scoreCategory :: Hand -> ScoreCard -> IO ScoreCard
scoreCategory (Hand hand) card = do
    putStr "Select category [a-m]: "
    hFlush stdout
    n <- getChar
    putStrLn ""
    case n of
        n | n `elem` (map sel1 card) -> 
            case getCategory n card of
                (cid, cname, NoValue, scoreFn) -> 
                    let values = map fst hand
                        updated = (cid, cname, Score (scoreFn values), scoreFn)
                    in return $ updateCategory updated card
                (_, _, Score _, _) -> do
                    putStrLn "> Category already scored"
                    scoreCategory (Hand hand) card
        otherwise -> do
            putStrLn "> Invalid category!"
            scoreCategory (Hand hand) card

-- TODO clean up
printRollResult :: Hand -> ScoreCard -> IO ()
printRollResult (Hand hand) card = do
    putStrLn "\n"
    mapM_ putCat unscored
    putStrLn ""

    where
        isScored (_, _, Score _, _) = True
        isScored (_, _, NoValue, _) = False
        unscored = filter (not . isScored) card

        putCat (cid, category, score, scoreFn) = putRow cid category score (scoreFn (map fst hand))
        putRow cid category NoValue handScore = putStrLn $ printf "%c) %-17s %3d" cid category handScore


printScoreCard :: ScoreCard -> IO ()
printScoreCard card = do
    putStrLn "\n"
    mapM_ putCat upper
    putRight "Sum" $ show sumUpper
    putRight "Bonus" $ show bonus
    emptyRow
    mapM_ putCat lower
    emptyRow
    putRight "Total" $ show (sumLower + sumUpper)

    where
        upper = take 6 card
        lower = drop 6 card
        sumUpper = total $ map sel3 upper
        sumLower = total $ map sel3 lower
        bonus = if sumUpper < 65 then 0 else 35
        putCat (cid, category, score, scoreFn) = putLeft category $ show score
        putLeft name value = putStrLn $ printf "[  %-17s %3s  ]" name value
        putRight name value = putStrLn $ printf "[  %17s %3s  ]" name value
        emptyRow = putStrLn "[                         ]"

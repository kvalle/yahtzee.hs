module Yahtzee.Game where

import Text.Printf
import System.IO (hFlush, stdout)
import System.Random (newStdGen)

import Yahtzee.Scoring
import Yahtzee.Hand

-- Types

data Try = FirstTry | SecondTry | FinalTry deriving (Eq, Ord, Enum)

instance Show Try where
    show FirstTry  = "1st"
    show SecondTry = "2nd"
    show FinalTry  = "Final"

-- Game interactions

play :: IO ()
play = do
    card <- playRounds 13 emptyScoreCard
    -- todo print final score
    return ()

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
    printScore hand card
    card <- chooseCategory hand card
    return card

chooseCategory :: Hand -> ScoreCard -> IO ScoreCard
chooseCategory (Hand hand) card = do
    putStr "Select category [a-m]: "
    hFlush stdout
    n <- getChar
    putStrLn ""
    case n of
        n    | n `elem` ['a'..'m'] ->
            case readCard n card of
                NoValue -> return $ updateCard n card $ Score (score n $ map fst hand)
                Score _ -> do
                    putStrLn "> Category already scored"
                    chooseCategory (Hand hand) card
        otherwise -> do
            putStrLn "> Invalid category!"
            chooseCategory (Hand hand) card

printScore :: Hand -> ScoreCard -> IO ()
printScore (Hand hand) card = do
    putStrLn $ category "a) Ones"   (ones card) (scoreN 1 values)
    putStrLn $ category "b) Twos"   (twos card) (scoreN 2 values)
    putStrLn $ category "c) Threes" (threes card) (scoreN 3 values)
    putStrLn $ category "d) Fours"  (fours card) (scoreN 4 values)
    putStrLn $ category "e) Fives"  (fives card) (scoreN 5 values)
    putStrLn $ category "f) Sixes"  (sixes card) (scoreN 6 values)

    putStrLn $ category "g) Small straight" (smallStraight card) (scoreSmallStraight values)
    putStrLn $ category "h) Large straight" (largeStraight card) (scoreLargeStraight values)
    putStrLn $ category "i) Three of a kind" (threeOfAKind card) (scoreThreeOfAKind values)
    putStrLn $ category "j) Four of a kind" (fourOfAKind card) (scoreFourOfAKind values)
    putStrLn $ category "k) Full house" (fullHouse card) (scoreFullHouse values)
    putStrLn $ category "l) Yahtzee" (yahtzee card) (scoreYahtzee values)
    putStrLn $ category "m) Chance" (chance card) (scoreChance values)
    putStrLn ""
    
    where
        values = map fst hand
        category category (Score score) _ =   printf "%-19s %2d" category score
        category category NoValue handScore = printf "%-19s %2d ?" category handScore

test = do
    let hand = Hand [(1,True), (1, True), (2, True), (1, True), (2, True)]
    printScore hand $ emptyScoreCard { twos = Score 4 }




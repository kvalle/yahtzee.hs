import System.IO

import Yahtzee.Game (play)

main = do
    -- to avoid waiting for newline when "holding" dice
    hSetBuffering stdin NoBuffering
    
    -- let the games begin!
    play

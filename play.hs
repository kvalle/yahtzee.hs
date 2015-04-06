import System.IO

import Yahtzee.Game (play)

main = do
    hSetBuffering stdin NoBuffering
    play

module Main where
    
import Game
import Render
import Graphics.Gloss

-- runs the gloss pure game function on the given functions from game and render
main :: IO ()
-- play :: Display -> Color -> Int -> world -> (world -> Picture) -> (Event -> world -> world) -> [Float -> world -> world] -> IO () 
main = play window backgroundColour 10 (initialGameState True True) gameToPicture updateKeyInput updateGameState

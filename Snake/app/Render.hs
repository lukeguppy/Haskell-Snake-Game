module Render where

import Graphics.Gloss
import Game

-- sets the window properties scaled by the number of rows, columns and cell size
window :: Display
window = InWindow "Snake" (columns * cellSize, rows * cellSize) (0, 0)

-- background colour of the game to draw given pictures on
backgroundColour :: Color
backgroundColour = makeColorI 50 50 50 255

-- colour of the edges
edgeColour :: Color
edgeColour = makeColorI 0 0 0 255

-- colour of the target
targetColor :: Color
targetColor = makeColorI 200 0 0 255

-- uses gloss function 'pictures' to convert a list of pictures generated from the current game state to one picture including:
-- target cell set to the given target colour
-- list of snake cells with the snake colour mapped to them
-- list of edge cells 
-- the gameover screen which returns the game over text if the game is over otherwise returns an empty set
gameToPicture :: GameState -> Picture
gameToPicture gameState = pictures $ [ setCellColour targetColor (getTarget gameState) ] ++
                                       snakeToPictures gameState ++
                                       drawEdges gameState ++
                                       gameOverScreen gameState ++
                                       scoreAsPicture gameState

-- converts the snake from the given game state to a list of pictures
-- works by mapping a function which sets cell colours to each cell of the snake
snakeToPictures :: GameState -> [Picture]
snakeToPictures gameState = fmap (setCellColour (makeColorI 0 150 0 255)) (getSnake gameState)

-- if there are edges active it maps set cell colour to each edge cell (all top, bottom and side cells) resulting in a 
-- list of cell pictures 
drawEdges :: GameState -> [Picture]
drawEdges gameState = if hasEdges gameState
                      then fmap (setCellColour edgeColour) [ (x,y) | x <- [0, columns - 1], y <- [0..rows- 1] ] ++
                           fmap (setCellColour edgeColour) [ (x,y) | x <- [0..columns - 1], y <- [0,rows - 1] ] 
                      else []

-- if the game is over it colours, scales and translates the game over text 
-- otherwise returns an empty set i.e. no picture
-- has two texts for space to start and e to add/remove edges
gameOverScreen :: GameState -> [Picture]
gameOverScreen gameState = [ color white 
                             $ translate (-ratio * 225) 0 
                             $ scale (ratio * 0.2) (ratio * 0.2) 
                             $ text "Press SPACE To Start New Game" 
                             | isGameOver gameState
                            ] ++ [
                             color white 
                             $ translate (-ratio * 205) (2 * fCellSize)
                             $ scale (ratio * 0.2) (ratio * 0.2) 
                             $ text "Press E To Add/Remove Edges" 
                             | isGameOver gameState
                           ]
                where ratio = (fColumns * (fCellSize / 20)) / 25 
                -- ratio scales the text proportionally to the number of collums

scoreAsPicture :: GameState  -> [Picture]
scoreAsPicture gameState = [ color white 
                            $ translate (-fCellSize * (fColumns / 2 - 1)) (fCellSize * (fRows / 2 - 2)) 
                            $ scale (fCellSize  * 0.008) (fCellSize  * 0.008) 
                            $ text $ show (getScore gameState) 
                           ]

-- sets a given cell to a given colour
-- Color -> Cell order so it can be mapped to cells
-- colours, scales and translates a solid rectangle to the position by the given coord
setCellColour :: Color -> Cell -> Picture
setCellColour colour (x, y) = color colour 
                              $ scale 1 (-1) -- since y is opposite (up is positive)
                              $ translate ((fromIntegral x + 0.5) * fCellSize  - fColumns * fCellSize * 0.5) ((fromIntegral y + 0.5) * fCellSize - fRows * fCellSize * 0.5) 
                              $ rectangleSolid fCellSize fCellSize 
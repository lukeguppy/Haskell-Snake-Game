module Game where

import Graphics.Gloss.Interface.Pure.Game
import System.Random

-- direction type can either be north (up), south (down)...
-- derives eq so can be compared when updating
data Direction = North | South | West | East deriving (Eq)
-- cell type is a pair of integer coordinats for a cell position
type Cell = (Int, Int)
-- Snake is a list of cells i.e. coords of each snake block
type Snake = [Cell]

-- height of window in cells including edges
columns :: Int
columns = 25

-- width of window in cells
rows :: Int
rows = 25

-- size of each 'cell' making up window i.e. snake block, edge, target block
cellSize :: Int
cellSize = 30

-- same values in float form
fCellSize :: Float
fCellSize = fromIntegral cellSize

fColumns :: Float
fColumns = fromIntegral columns

fRows :: Float
fRows = fromIntegral rows

-- represents the current state of the game including:
-- snake blocks, current direction, whether the game is over, the target cell, the random stdgen to generate a new target location
data GameState = GameState {  getSnake :: Snake
                            , getDirection :: Direction
                            , isGameOver :: Bool
                            , getTarget :: Cell
                            , getRandomStdGen :: StdGen
                            , getScore :: Int
                            , hasEdges :: Bool 
                           }

-- updates the game state based on:
-- if the game is over, 
-- updated target position (if hit or not),
-- new snake if target hit,
-- updated directions from input,
-- new stdgen for each gamestate to generate new target locations
updateGameState :: Float -> GameState -> GameState
updateGameState _ gameState = if isGameOver gameState
                              then gameState
                              -- updates all fields except whether it has edges or not
                              else gameState {  getSnake = updatedSnake
                                              , isGameOver = checkGameOver updatedSnake (hasEdges gameState)
                                              , getTarget = updatedTarget
                                              , getRandomStdGen = newStdGen
                                              , getScore = updatedScore
                                             }
                where   target = getTarget gameState -- current target position
                        updatedSnake = moveSnake gameState (getDirection gameState) target -- shifts snake along keeping the last block if the target is hit
                        (newTarget, newStdGen) = generateNewTargetAndStdGen updatedSnake (getRandomStdGen gameState) -- generate a new target position if it needs to be used
                        -- if it has hit a target then use new target otherwise use new one
                        updatedTarget = if head updatedSnake == target 
                                        then newTarget
                                        else target
                        -- if it has hit a target then increment score                     
                        updatedScore =  if head updatedSnake == target
                                        then getScore gameState + 1
                                        else getScore gameState

-- the initial state of the game where:
-- the snake is 2 cells long, facing north, the game is over and the target is in the centre
initialGameState :: Bool -> Bool -> GameState
initialGameState gameOver edgesOn = GameState {  getSnake = [ (snakeX, snakeY), (snakeX, snakeY + 1) ]
                                               , getDirection = North
                                               , isGameOver = gameOver
                                               , getTarget = (snakeX, snakeY - 3)
                                               , getRandomStdGen = mkStdGen 100
                                               , getScore = 0
                                               , hasEdges = edgesOn
                                              }
                        where   snakeX = columns `div` 2
                                snakeY = rows `div` 2 + 2

-- if the new head position of the snake is the same as the target then the head position is consed with the original snake
-- otherwise the new head position is consed with the original snake excluding its end cell i.e. shifting it all one block
moveSnake ::GameState  -> Direction -> Cell -> Snake
moveSnake gameState direction target = if newHead == target
                                       then newHead : snake
                                       else newHead : init snake -- init inclues all in list except last
-- mod used to teleport to other side of screen if edges aren't active otherwise hits edge before
                        where   newHead = ((dx + sx) `mod` columns, (dy + sy) `mod` rows)
                                (sx, sy) = head snake
                                (dx, dy) = directionToVector direction
                                snake = getSnake gameState

-- converts each direction to a vector (to add to the snake head to move it)
directionToVector :: Direction -> (Int, Int)
directionToVector North = (0, -1)
directionToVector South = (0, 1)
directionToVector West = (-1, 0)
directionToVector East = (1, 0)

-- returns whether the snake has crashed into an edge or its body
checkGameOver :: Snake -> Bool -> Bool
-- if edges are activated then it checks if it has hit an edge or its tail
checkGameOver snake True = headX == 0 || headY == 0 || headX == columns - 1 || headY == rows - 1 || (headX, headY) `elem` tail snake
                where   (headX, headY) = head snake
-- if edges not activated then it just checks if it has hit its tail
checkGameOver snake False = head snake `elem` tail snake

-- uses the current StdGen (from random) to generate a new target location
-- if the new location is a snake cell then it uses recursion to generate a new target
-- the result is the new target location and a new stdgen for next time a new target is needed
generateNewTargetAndStdGen :: Snake -> StdGen -> (Cell, StdGen)
generateNewTargetAndStdGen snake stdGen = if newTarget `elem` snake
                                          then generateNewTargetAndStdGen snake stdGen3
                                          else ((newTargetX, newTargetY), stdGen3)
                -- generate random x and y ints using the stdGen and the newly generated stdGen from that result
                where   (newTargetX, stdGen2) = randomR (1, columns - 1) stdGen
                        (newTargetY, stdGen3) = randomR (1, rows - 1) stdGen2
                        newTarget = (newTargetX, newTargetY)

-- method for updating the game state for given key inputs
-- for up, down, left, right arrows then if the current direction is not the opposite direction it updates 
-- the getDirection field of the game state to the given direction
updateKeyInput :: Event -> GameState -> GameState
updateKeyInput (EventKey (SpecialKey KeyLeft ) Down _ _) gameState = if getDirection gameState == East
                                                                     then gameState
                                                                     else gameState { getDirection = West }

updateKeyInput (EventKey (SpecialKey KeyRight) Down _ _) gameState = if getDirection gameState == West
                                                                     then gameState
                                                                     else gameState { getDirection = East }

updateKeyInput (EventKey (SpecialKey KeyUp   ) Down _ _) gameState = if getDirection gameState == South
                                                                     then gameState
                                                                     else gameState { getDirection = North }

updateKeyInput (EventKey (SpecialKey KeyDown ) Down _ _) gameState = if getDirection gameState == North
                                                                     then gameState
                                                                     else gameState { getDirection = South }

-- for a space input if the game is over it starts it at the initial game state
-- otherwise it does nothing and returns the current game state
updateKeyInput (EventKey (SpecialKey KeySpace) Down _ _) gameState = if isGameOver gameState
                                                                     then initialGameState False (hasEdges gameState)
                                                                     else gameState

-- if e pressed during game over screen then add or remove edges
updateKeyInput (EventKey (Char 'e') Down _ _) gameState =            if isGameOver gameState
                                                                     then gameState { hasEdges = not (hasEdges gameState) }
                                                                     else gameState

-- any other inputs do not affect the gameState
updateKeyInput _ gameState = gameState
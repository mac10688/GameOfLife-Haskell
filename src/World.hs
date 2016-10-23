module World (World, getWorld, buildWorld, getNeighbors, lookupCell) where

import Cell
import Position

newtype World = W {getWorld :: [[Cell]]}

instance Show World where
  show (W cells) = cells >>= (\row -> show row ++ "\n")

buildWorld :: Int -> Int -> [Position] -> World
buildWorld col row liveCells = W [
  [ if mkPosition r c `elem` liveCells then Live else Dead
    | r <- [0..(row-1)]]
    | c <- [0..(col-1)]]

getNeighbors :: World -> Position -> [Cell]
getNeighbors world position = let
  (row, col) = getPosition position
  neighborPositions = [
                         mkPosition (row + 1) (col + 1)
                        ,mkPosition (row + 1) col
                        ,mkPosition (row + 1) (col - 1)
                        ,mkPosition row (col + 1)
                        ,mkPosition row (col - 1)
                        ,mkPosition (row - 1) (col + 1)
                        ,mkPosition (row - 1) col
                        ,mkPosition (row - 1) (col - 1)
                      ]
  in map (lookupCell world) neighborPositions

lookupCell :: World -> Position -> Cell
lookupCell (W cells) position = let
  (row, col) = getPosition position
  colMax = length $ cells !! 0
  rowMax = length cells
  in cells !! (row `mod` rowMax) !! (col `mod` colMax)

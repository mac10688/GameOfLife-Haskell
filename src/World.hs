module World (World, getWorld, deadWorld, buildWorld, getNeighbors, lookupCell) where

import Cell

newtype World = W {getWorld :: [[Cell]]}
instance Show World where
  show (W cells) = cells >>= (\row -> show row ++ "\n")

deadWorld :: Int -> Int -> World
deadWorld row col = W [[Dead | _ <- [0..row]] | _ <- [0..col]]

buildWorld :: Int -> Int -> [(Int, Int)] -> World
buildWorld col row liveCells = W [
  [ if ((r,c) `elem` liveCells) then Live else Dead
    | r <- [0..(row-1)]]
    | c <- [0..(col-1)]]

getNeighbors :: World -> (Int, Int) -> [Cell]
getNeighbors world (row, col) = let
  neighborPositions = [(row + 1, col + 1)
                      ,(row + 1, col)
                      ,(row + 1, col - 1)
                      ,(row, col + 1)
                      ,(row, col - 1)
                      ,(row - 1, col + 1)
                      ,(row - 1, col)
                      ,(row - 1, col - 1)
                      ]
  in map (lookupCell world) neighborPositions

lookupCell :: World -> (Int, Int) -> Cell
lookupCell (W cells) (row, col) = let
  colMax = length $ cells !! 0
  rowMax = length cells
  in cells !! (row `mod` rowMax) !! (col `mod` colMax)

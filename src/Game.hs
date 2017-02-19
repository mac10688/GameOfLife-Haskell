module Game where

import Cell

newState :: Cell -> [Cell] -> Cell
newState Live neighbors = let
  liveCells = length (filter isLive neighbors)
  in if (liveCells == 2 || liveCells == 3)
    then Live
    else Dead
newState Dead neighbors =
  if (3 == length (filter isLive neighbors))
    then Live
    else Dead

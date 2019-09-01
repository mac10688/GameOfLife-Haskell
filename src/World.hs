module World (World, buildWorld, iterateWorld, Position(..), getWorld, toList, Cell(..), getNeighbors, isLive, newState) where

import Prelude hiding (lookup)

import Data.Map.Strict as Map (fromList, Map, lookup, mapWithKey, toAscList)
import Data.Maybe (catMaybes)

data Position = Position { row :: Integer
                         , col :: Integer
                         } deriving (Show, Eq, Ord)

newtype World = W {getWorld :: Map Position Cell}
  deriving (Eq, Show)

toList :: World -> [(Position, Cell)]
toList world = toAscList $ getWorld world

buildWorld :: Integer -> Integer -> [Position] -> World
buildWorld row' col' liveCells = let
  cells = [ if (Position r c) `elem` liveCells then (Position r c, Live) else (Position r c, Dead) 
    | c <- [ 1..col' ], r <- [ 1..row' ]]
  in
    W $ fromList cells

iterateWorld :: World -> World
iterateWorld world =
  let
    worldMap = getWorld world
  in
    W $ mapWithKey (iterateCell worldMap) worldMap

iterateCell :: Map Position Cell -> Position -> Cell -> Cell
iterateCell worldMap position cell = 
  let 
    neighbors = getNeighbors worldMap position 
  in
    newState cell neighbors

getNeighbors :: Map Position Cell -> Position -> [Cell]
getNeighbors worldMap position = 
  let
    neighborPositions = [ lookup (Position r c) worldMap 
      | c <- [(col position -1) .. (col position + 1)]
      , r <- [(row position -1) .. (row position + 1)]
      , not (c == (col position) && r == (row position)) ]
  in 
    catMaybes neighborPositions

data Cell = Live | Dead
    deriving (Eq, Show)

isLive :: Cell -> Bool
isLive Live = True
isLive _    = False

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

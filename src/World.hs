module World (World, buildWorld, iterateWorld, Position(..), getWorld, toList, Cell(..), getNeighbors, isLive, newState) where

import Prelude hiding (lookup)

import Data.Map.Strict as Map (fromList, Map, lookup, mapWithKey, toAscList)
import Data.Maybe (catMaybes)
--import Data.List (groupBy, intercalate)

data Position = Position { row :: Integer
                         , col :: Integer
                         } deriving (Show, Eq, Ord)

newtype World = W {getWorld :: Map Position Cell}
  deriving (Eq, Show)

--instance Show World where
  --show (W worldMap) =  unwords $ intercalate ["\n"] $ showCell $ groupByRow $ toAscList worldMap
    --where
      --groupByRow :: [(Position, Cell)] -> [[(Position, Cell)]]
      --groupByRow worldArray = groupBy (\ (pos1, _) (pos2, _) -> (row pos1) == (row pos2)) worldArray
      --showCell :: [[(Position, Cell)]] -> [[String]]
      --showCell = (map . map) (show . snd)

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
getNeighbors worldMap position = let
  neighborPositions = [
                         lookup (Position (row position + 1) (col position + 1)) worldMap
                        ,lookup (Position (row position + 1) (col position)) worldMap
                        ,lookup (Position (row position + 1) (col position - 1)) worldMap
                        ,lookup (Position (row position) (col position + 1)) worldMap
                        ,lookup (Position (row position) (col position - 1)) worldMap
                        ,lookup (Position (row position - 1) (col position + 1)) worldMap
                        ,lookup (Position (row position - 1) (col position)) worldMap
                        ,lookup (Position (row position - 1) (col position - 1)) worldMap 
                      ]
  in catMaybes neighborPositions

data Cell = Live | Dead
    deriving (Eq)

instance Show Cell where
  show Live = "O"
  show Dead = "X"

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

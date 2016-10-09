module Main where

import Test.Hspec

import Cell
import World
import Game

createNeighbors :: Int -> [Cell]
createNeighbors liveCells = (replicate liveCells Live) ++ (replicate (8 - liveCells) Dead)

main :: IO ()
main = hspec $ do
  describe "buildWorld" $ do
    it "places live cells: top left" $ do
      let world = buildWorld 3 3 [(0,0)]
      [[Live,Dead,Dead],[Dead,Dead,Dead],[Dead,Dead,Dead]] == getWorld world
    it "places live cells: bottom right" $ do
      let world = buildWorld 3 3 [(2,2)]
      [[Dead,Dead,Dead],[Dead,Dead,Dead],[Dead,Dead,Live]] == getWorld world
    it "can make rectangular worlds" $ do
      let world = buildWorld 1 3 []
      [[Dead, Dead, Dead]] == getWorld world
  describe "getNeighbors" $ do
    it "returns 8 cells" $ do
      let world = buildWorld 1 1 []
      8 == length (getNeighbors world (0,0))
    it "provides status of surrounding cells" $ do
      let world = buildWorld 3 3 [(0,0)]
      let neighbors = getNeighbors world (1,1)
      let liveCells = filter isLive neighbors
      let deadCells = filter (not . isLive) neighbors
      (length liveCells == 1) && (length deadCells == 7)
  describe "newState" $ do
    it "rule 1: live cell dies with fewer than 2 live neighbors - 0 neighbors" $ do
      let neighbors = createNeighbors 0
      Dead == newState Live neighbors
    it "rule 1: live cell dies with fewer than 2 live neighbors - 1 neighbor" $ do
      let neighbors = createNeighbors 1
      Dead == newState Live neighbors
    it "rule 2: live cell lives with 2 or 3 neighbors - 2 neighbors" $ do
      let neighbors = createNeighbors 2
      Live == newState Live neighbors
    it "rule 2: live cell lives with 2 or 3 neighbors - 3 neighbors" $ do
      let neighbors = createNeighbors 3
      Live == newState Live neighbors
    it "rule 3: live cell dies with more than 3 neighbors - 4 neighbors" $ do
      let neighbors = createNeighbors 4
      Dead == newState Dead neighbors
    it "rule 3: live cell dies with more than 3 neighbors - 5 neighbors" $ do
      let neighbors = createNeighbors 5
      Dead == newState Dead neighbors
    it "rule 3: live cell dies with more than 3 neighbors - 6 neighbors" $ do
      let neighbors = createNeighbors 6
      Dead == newState Dead neighbors
    it "rule 3: live cell dies with more than 3 neighbors - 7 neighbors" $ do
      let neighbors = createNeighbors 7
      Dead == newState Dead neighbors
    it "rule 3: live cell dies with more than 3 neighbors - 8 neighbors" $ do
      let neighbors = createNeighbors 8
      Dead == newState Dead neighbors
    it "rule 4: dead cells repopulate with exactly 3 neighbors" $ do
      let neighbors = createNeighbors 3
      Live == newState Dead neighbors

module Main where

import Test.Hspec

import Data.Map.Strict

import World

main :: IO ()
main = hspec $ do
  describe "buildWorld" $ do
    it "places live cells: top left" $ do
      let world = buildWorld 3 3 [Position 1 1 ]
      fromList [(Position 1 1, Live), (Position 1 2, Dead), (Position 1 3, Dead),
                (Position 2 1, Dead), (Position 2 2, Dead), (Position 2 3, Dead),
                (Position 3 1, Dead), (Position 3 2, Dead), (Position 3 3, Dead)] == getWorld world
    it "places live cells: bottom right" $ do
      let world = buildWorld 3 3 [Position 3 3]
      fromList [(Position 1 1, Dead), (Position 1 2, Dead), (Position 1 3, Dead),
                (Position 2 1, Dead), (Position 2 2, Dead), (Position 2 3, Dead),
                (Position 3 1, Dead), (Position 3 2, Dead), (Position 3 3, Live)] == getWorld world
    it "can make rectangular worlds" $ do
      let world = buildWorld 1 3 []
      fromList [(Position 1 1, Dead), (Position 1 2, Dead), (Position 1 3, Dead)] == getWorld world
  describe "getNeighbors" $ do
    it "returns 8 cells" $ do
      let world = buildWorld 1 1 []
      0 == (length $ getNeighbors (getWorld world) $ Position 1 1)
    it "provides status of surrounding cells" $ do
      let world = buildWorld 3 3 [Position 1 1]
      let neighbors = getNeighbors (getWorld world) $ Position 2 2
      let liveCells = Prelude.filter isLive neighbors
      let deadCells = Prelude.filter (not . isLive) neighbors
      (Prelude.length liveCells == 1) && (Prelude.length deadCells == 7)
  describe "newState" $ do
    it "rule 1: live cell dies with fewer than 2 live neighbors - 0 neighbors" $ do
      Dead == newState Live []
    it "rule 1: live cell dies with fewer than 2 live neighbors - 1 neighbor" $ do
      Dead == newState Live [Live]
    it "rule 2: live cell lives with 2 or 3 neighbors - 2 neighbors" $ do
      Live == newState Live [Live, Live]
    it "rule 2: live cell lives with 2 or 3 neighbors - 3 neighbors" $ do
      Live == newState Live [Live, Live, Live]
    it "rule 3: live cell dies with more than 3 neighbors - 4 neighbors" $ do
      Dead == newState Live [Live, Live, Live, Live]
    it "rule 3: live cell dies with more than 3 neighbors - 5 neighbors" $ do
      Dead == newState Live [Live, Live, Live, Live, Live]
    it "rule 3: live cell dies with more than 3 neighbors - 6 neighbors" $ do
      Dead == newState Live [Live, Live, Live, Live, Live, Live]
    it "rule 3: live cell dies with more than 3 neighbors - 7 neighbors" $ do
      Dead == newState Live [Live, Live, Live, Live, Live, Live, Live]
    it "rule 3: live cell dies with more than 3 neighbors - 8 neighbors" $ do
      Dead == newState Live [Live, Live, Live, Live, Live, Live, Live, Live]
    it "rule 4: dead cells repopulate with exactly 3 neighbors" $ do
      Live == newState Dead [Live, Live, Live]
  describe "iterateWorld" $ do
    it "still life: block" $ do
      let initialState = buildWorld 4 4 [Position 2 2, Position 2 3, Position 3 2, Position 3 3]
          nextState = iterateWorld initialState
      initialState == nextState
    it "still life: beehive" $ do
      let initialState = buildWorld 6 6 [Position 1 2, Position 1 3, Position 2 1, Position 2 4, Position 3 2, Position 3 3]
          nextState = iterateWorld initialState
      initialState == nextState
    it "oscillator: blinker" $ do
      let initialState = buildWorld 5 5 [ Position 3 1, Position 3 2, Position 3 3]
          nextState = iterateWorld initialState
          expectedState = buildWorld 5 5 [ Position 2 2, Position 3 2, Position 4 2]
      nextState == expectedState
    --it "oscillator: blinker repeats" $ do
      --let initialState = buildWorld 5 5 [(2,2), (1,2), (3,2)]
      --initialState == (iterateWorld $ iterateWorld initialState)
    it "oscillator: toad" $ do
      let initialState = buildWorld 6 6 [ Position 1 2, Position 2 2, Position 2 3, Position 3 2, Position 3 3, Position 4 3]
          nextState = iterateWorld initialState
          expectedState = buildWorld 6 6 [ Position 1 2, Position 1 3, Position 2 1, Position 3 4, Position 4 2, Position 4 3]
      nextState == expectedState
    --it "oscillator: toad repeats" $ do
      --let initialState = buildWorld 6 6 [(1,2),(1,3),(2,4),(3,1),(4,2),(4,3)]
      --initialState == (iterateWorld $ iterateWorld initialState)

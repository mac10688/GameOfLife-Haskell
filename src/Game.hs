module Game where

import World
import Cell

createDeadWorld :: World
createDeadWorld = [[Dead | y <- [0..9]] | x <- [0..9]]

module Game where

import World
import Cell

createDeadWorld :: World
createDeadWorld = [[Dead | _ <- [0..9]] | _ <- [0..9]]

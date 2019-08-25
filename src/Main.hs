module Main where

import System.Console.ANSI
import Control.Concurrent

import World (World, buildWorld, iterateWorld)
import Position (Position, mkPosition)

import Control.Monad.IO.Class

import UI.NCurses

main :: IO ()
main = runCurses $ do
  setEcho False
  w <- defaultWindow
  let startWorld = buildWorld 11 55 [ mkPosition 0 5
                                    , mkPosition 0 6
                                    , mkPosition 1 6
                                    , mkPosition 2 6
                                    , mkPosition 1 7
                                    , mkPosition 2 8
                                    , mkPosition 3 8]
  waitFor startWorld w (\ev -> ev == EventCharacter 'q')

waitFor :: World -> Window -> (Event -> Bool) -> Curses ()
waitFor world window event = loop world where
  loop world' = do
   let newWorld = iterateWorld world'
   updateWindow window $ do
      clear
      moveCursor 0 0
      drawString $ show newWorld
   liftIO $ threadDelay 1000000
    
   render
   loop newWorld

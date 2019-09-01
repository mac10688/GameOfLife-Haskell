module Main where

import System.Console.ANSI
import Control.Concurrent

import World (World, buildWorld, iterateWorld, Position(..), Cell(..), getWorld, toList)

import Control.Monad.IO.Class

import UI.NCurses

main :: IO ()
main = runCurses $ do
  setEcho False
  w <- defaultWindow
  let startWorld = buildWorld 6 6 [ Position 1 2, Position 2 2, Position 2 3, Position 3 2, Position 3 3, Position 4 3]
  waitFor startWorld w (\ev -> ev == EventCharacter 'q')

waitFor :: World -> Window -> (Event -> Bool) -> Curses ()
waitFor world window event = loop world where
  loop world' = do
   let newWorld = iterateWorld world'
   updateWindow window $ do
      clear
      let worldMap = toList newWorld
      mapM_ drawCellAtPosition worldMap
                                                                  
      --moveCursor 0 0
      --drawString $ show newWorld
    
   render
   liftIO $ threadDelay 1000000
   loop newWorld

drawCellAtPosition :: (Position, Cell) -> Update ()
drawCellAtPosition (position, cell) = do
  moveCursor (row position) (col position)
  case cell of
    Live -> drawGlyph $ Glyph '\x25A1' []
    Dead -> drawGlyph $ Glyph '\x25A0' []

  --drawString $ show cell


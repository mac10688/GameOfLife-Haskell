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
  let startWorld = buildWorld 15 40 [ Position 6 2, Position 7 2, Position 6 3, Position 7 3,
                                      Position 4 14, Position 4 15, Position 5 13, Position 5 17, Position 6 12, Position 6 18, Position 7 12, Position 7 16, Position 7 18, Position 7 19, Position 8 12, Position 8 18, Position 9 13, Position 9 17, Position 10 14, Position 10 15,
                                      Position 2 26, Position 3 24, Position 3 26, Position 4 22, Position 4 23, Position 5 22, Position 5 23, Position 6 22, Position 6 23, Position 7 24, Position 7 26, Position 8 26,
                                      Position 4 36, Position 4 37, Position 5 36, Position 5 37]
  waitFor startWorld w (\ev -> ev == EventCharacter 'q')

waitFor :: World -> Window -> (Event -> Bool) -> Curses ()
waitFor world window event = loop world where
  loop world' = do
   let newWorld = iterateWorld world'
   updateWindow window $ do
      clear
      let worldMap = toList newWorld
      mapM_ drawCellAtPosition worldMap
   render
   liftIO $ threadDelay 100000
   loop newWorld

drawCellAtPosition :: (Position, Cell) -> Update ()
drawCellAtPosition (position, cell) = do
  moveCursor (row position) (col position)
  case cell of
    Live -> drawGlyph $ Glyph '\x25A1' []
    Dead -> drawGlyph $ Glyph '\x25A0' []

module Main where

import System.Console.ANSI
import Control.Concurrent

import World (World, buildWorld, iterateWorld)
import Position (Position, mkPosition)

import Reactive.Banana
import Reactive.Banana.Frameworks

main :: IO ()
main = do
  (addHandler, trigger) <- newAddHandler 
  network <- setupNetwork addHandler
  actuate network
  eventLoop trigger

eventLoop :: (() -> IO()) -> IO ()
eventLoop trigger = loop
  where
  loop = do
    trigger ()
    threadDelay 1000000
    loop

setupNetwork :: AddHandler () -> IO EventNetwork
setupNetwork addHandler = compile $ do
  etick <- fromAddHandler addHandler
  let startWorld = buildWorld 5 5 [mkPosition 2 2, mkPosition 1 2, mkPosition 3 2]
  eWorld <- accumE startWorld $ iterateWorld <$ etick
  reactimate $ fmap (\world -> clearScreen >> print world) eWorld

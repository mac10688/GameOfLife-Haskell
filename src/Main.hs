module Main where

import System.Console.ANSI
import Control.Concurrent

import World (World, buildWorld, iterateWorld)
import Position (Position, mkPosition)

import Reactive.Banana
import Reactive.Banana.Frameworks

main :: IO ()
main = do
 --Create an add handler and the trigger
 --The add handler will be wired into the network
 --The trigger is a function that just needs one argument
 --When it recieves the argument the addHandler will be fired
  (addHandler, trigger) <- newAddHandler 
  network <- setupNetwork addHandler
  --start the network.
  --Have the listeners listen for the function to be fired
  actuate network
  eventLoop trigger

eventLoop :: (() -> IO()) -> IO ()
eventLoop trigger = loop
  where
  loop = do
    --fire the function
    trigger ()
    --wait 1 second
    threadDelay 1000000
    loop

setupNetwork :: AddHandler () -> IO EventNetwork
setupNetwork addHandler = compile $ do
  --Get the event. I really don't know how to explain this.
  etick <- fromAddHandler addHandler
  --Creates an initial world to start
  let startWorld = buildWorld 5 5 [mkPosition 2 2, mkPosition 1 2, mkPosition 3 2]
  --accumE will take an initial world.
  --The next part will be an Event function that will iterate
  --to the next world for each event fired.
  eWorld <- accumE startWorld $ iterateWorld <$ etick
  --Each time there is an eWorld event, print it.
  --That's what reactimate does.
  reactimate $ fmap (\world -> clearScreen >> print world) eWorld

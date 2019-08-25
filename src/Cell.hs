module Cell where

data Cell = Live | Dead
    deriving (Eq)

instance Show Cell where
  show Live = "O"
  show Dead = " "

isLive :: Cell -> Bool
isLive Live = True
isLive _    = False

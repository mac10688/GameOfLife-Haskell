module Cell where

data Cell = Live | Dead
    deriving (Eq, Show)

isLive :: Cell -> Bool
isLive Live = True
isLive _    = False

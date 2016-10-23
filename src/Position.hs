module Position (Position, mkPosition, getPosition) where

newtype Position = P {getPosition :: (Int, Int)}
    deriving (Eq, Show)

mkPosition :: Int -> Int -> Position
mkPosition x y =  P (x, y)

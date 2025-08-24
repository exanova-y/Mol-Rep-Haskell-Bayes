module Coordinate where

data Coordinate = Coordinate
    { x :: Double
    , y :: Double
    , z :: Double
    } deriving (Show, Eq, Read)
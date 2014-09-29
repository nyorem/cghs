-- | Functions and types related to the orientation (2 vectors or 3 points).
module Cghs.Types.Orientation
where

-- | Orientation type.
data Orientation = LeftTurn
                  | RightTurn
                  | Collinear
                 deriving (Show, Eq)


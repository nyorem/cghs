module Math.Types.Orientation
where

-- orientation
data Orientation = LeftTurn
                  | RightTurn
                  | Collinear
                 deriving (Show, Eq)


module Math.Types.Segment2
where

import Math.Types.PointVector2

data Segment2 a = Segment2 {src :: Point2 a,
                            dst :: Point2 a} deriving (Show, Eq)


-- | Segments in two dimensions.
module Math.Types.Segment2
where

import Math.Types.PointVector2

-- | The 2D segment data type.
data Segment2 a = Segment2 {src :: Point2 a,
                            dst :: Point2 a} deriving (Show, Eq)


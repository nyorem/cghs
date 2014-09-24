-- | Points and vectors in two dimensions.
module Math.Types.PointVector2
where

import Data.Monoid

import Math.Utils.Tuple

-- | 2D point data type.
newtype Point2 a = Point2 { getPoint2 :: (a, a) } deriving (Eq, Show)

-- | 2D vector data type.
newtype Vector2 a = Vector2 { getVector2 :: (a, a) } deriving (Eq, Show)

-- | Applies a function to a point.
instance Functor Point2 where
    fmap f p = Point2 $ f >< (getPoint2 p)

-- | The origin point.
origin :: (Num a) => Point2 a
origin = Point2 (0, 0)

-- | ith coordinate of a point.
ith :: Int -> Point2 a -> a
ith 1 = fst . getPoint2
ith 2 = snd . getPoint2
ith _ = error "Coordinate index out of bounds"

-- | X-coordinate of a point.
x :: Point2 a -> a
x = ith 1

-- | Y-coordinate of a point.
y :: Point2 a -> a
y = ith 2

-- | Applies a function to a vector.
instance Functor Vector2 where
    fmap f p = Vector2 $ f >< (getVector2 p)

-- | Vectors define a monoid.
instance (Num a) => Monoid (Vector2 a) where
    mempty = originv
    mappend = (<+>)

-- | The origin vector.
originv :: (Num a) => Vector2 a
originv = Vector2 (0, 0)

-- | ith coordinate of a vector.
ithv :: Int -> Vector2 a -> a
ithv 1 = fst . getVector2
ithv 2 = snd . getVector2
ithv _ = error "Coordinate index out of bounds"

-- | X-coordinate of a vector.
xv :: Vector2 a -> a
xv = ithv 1

-- | Y-coordinate of a vector.
yv :: Vector2 a -> a
yv = ithv 2

-- | Squared norm of a vector.
squaredNorm :: (Num a) => Vector2 a -> a
squaredNorm u = u <.> u

-- | Opposite of a vector.
negateV :: (Num a) => Vector2 a -> Vector2 a
negateV = fmap negate

-- | Addition of a point and a vector gives a point.
(.+>) :: (Num a) => Point2 a -> Vector2 a -> Point2 a
p .+> v = Point2 (x p + xv v, y p + yv v)

-- | Subtraction of two points gives a vector.
(.-.) :: (Num a) => Point2 a -> Point2 a -> Vector2 a
p .-. q = Vector2 (x p - x q, y p - y q)

-- | Addition of two vectors gives a vector.
(<+>) :: (Num a) => Vector2 a -> Vector2 a -> Vector2 a
u <+> v = Vector2 (xv u + xv v, yv u + yv v)

-- | Subtraction of two vectors gives a vector.
(<->) :: (Num a) => Vector2 a -> Vector2 a -> Vector2 a
u <-> v = negateV (u <+> v)

-- | Dot product of two vectors.
(<.>) :: (Num a) => Vector2 a -> Vector2 a -> a
u <.> v = xv u * xv v + yv u * yv v

-- | Determinant of two vectors.
(<^>) :: (Num a) => Vector2 a -> Vector2 a -> a
u <^> v = xv u * yv v - yv u * xv v

-- | Collinearity test.
collinear :: (Eq a, Num a) => Vector2 a -> Vector2 a -> Bool
u `collinear` v = u <^> v == 0


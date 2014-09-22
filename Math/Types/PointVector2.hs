module Math.Types.PointVector2
where

import Math.CGUtils

newtype Vector2 a = Vector2 { getVector2 :: (a, a) } deriving (Eq, Show)
newtype Point2 a = Point2 { getPoint2 :: (a, a) } deriving (Eq, Show)

instance Functor Point2 where
    fmap f p = Point2 $ f >< (getPoint2 p)

ith :: Int -> Point2 a -> a
ith 1 = fst . getPoint2
ith 2 = snd . getPoint2
ith _ = error "Coordinate index out of bounds"

x, y :: Point2 a -> a
x = ith 1
y = ith 2

instance Functor Vector2 where
    fmap f p = Vector2 $ f >< (getVector2 p)

ithv :: Int -> Vector2 a -> a
ithv 1 = fst . getVector2
ithv 2 = snd . getVector2
ithv _ = error "Coordinate index out of bounds"

xv, yv :: Vector2 a -> a
xv = ithv 1
yv = ithv 2

squaredNorm :: (Num a) => Vector2 a -> a
squaredNorm v = xv v * xv v + yv v * yv v

(.+>) :: (Num a) => Point2 a -> Vector2 a -> Point2 a
p .+> v = Point2 (x p + xv v, y p + yv v)

(.-.) :: (Num a) => Point2 a -> Point2 a -> Vector2 a
p .-. q = Vector2 (x p - x q, y p - y q)

(<+>) :: (Num a) => Vector2 a -> Vector2 a -> Vector2 a
u <+> v = Vector2 (xv u + xv v, yv u + yv v)

negateV :: (Num a) => Vector2 a -> Vector2 a
negateV = fmap negate

(<->) :: (Num a) => Vector2 a -> Vector2 a -> Vector2 a
u <-> v = negateV (u <+> v)


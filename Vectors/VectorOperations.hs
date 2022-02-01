module VectorOperations where

data Vector a
    =  TwoD a a
    |  ThreeD a a a
    deriving Show

-- An operation to handle the addition of vectors to each other. Tried using Maybe/Nothing/Just instead of error but couldn't. Research later.
vectorAddition :: Real a => Vector a -> Vector a -> Maybe (Vector a)
vectorAddition (TwoD x y) (TwoD a b) = Just(TwoD (x+a) (y+b))
vectorAddition (ThreeD x y z) (ThreeD a b c) = Just(ThreeD (x+a) (y+b) (z+c))
vectorAddition (ThreeD x y z) (TwoD a b) = Nothing 
vectorAddition (TwoD a b) (ThreeD x y z) = Nothing 

-- An operation to handle the multiplication of vectors to scalar constants
scalarMultiplication :: Real a => a -> Vector a -> Vector a
scalarMultiplication a (TwoD x y) = TwoD (x*a) (y*a)
scalarMultiplication a (ThreeD x  y  z) = ThreeD (x*a) (y*a) (z*a)

-- An operation to get the length of a vector.
vectorLength :: Floating a => Vector a -> a
vectorLength (TwoD x y) =  sqrt((x^2)+(y^2))
vectorLength (ThreeD x y z) = sqrt(x^2+y^2+z^2)

-- An operation to get the distance between two vectors
vectorDistance :: Floating a => Vector a -> Vector a -> Maybe a
vectorDistance (TwoD x y) (TwoD a b) = Just(sqrt((x-a)^2+(y-b)^2))
vectorDistance (ThreeD x y z) (ThreeD a b c) = Just(sqrt((x-a)^2+(y-b)^2+(z-c)^2))
vectorDistance (TwoD x y) (ThreeD a b c) =  Nothing
vectorDistance (ThreeD x y z) (TwoD a b) = Nothing
-- An Operation to get the dot product between two vectors
vectorDotProduct:: Num a => Vector a -> Vector a -> Maybe a
vectorDotProduct (TwoD x y) (TwoD a b) = Just((x*a)+(y*b))
vectorDotProduct (ThreeD x y z) (ThreeD a b c) = Just((x*a)+(y*b)+(z*c))
vectorDotProduct (TwoD x y) (ThreeD a b c) =  Nothing
vectorDotProduct (ThreeD x y z) (TwoD a b) = Nothing

-- An operartion to rotate the vector
vectorRotation:: Floating a => a -> Vector a -> Vector a
vectorRotation a (TwoD x y) = TwoD (x*cos a - y*sin a) (x*sin a + y*cos a)
vectorRotation a (ThreeD x y z) = error "Will use matrices for that later"
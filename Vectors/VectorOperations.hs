module VectorOperations where
import GHC.Base (VecElem(Int16ElemRep))
import TcCanonical (StopOrContinue)

data Vector
    =  TwoD Float  Float 
    |  ThreeD Float Float Float 
    deriving Show

-- An operation to handle the addition of vectors to each other. Tried using Maybe/Nothing/Just instead of error but couldn't. Research later.
vectorAddition :: Vector -> Vector -> Vector
vectorAddition (TwoD x y) (TwoD a b) = TwoD (x+a) (y+b)
vectorAddition (ThreeD x y z) (ThreeD a b c) = ThreeD (x+a) (y+b) (z+c)
vectorAddition (ThreeD x y z) (TwoD a b) = error "Can't be different dimensions"
vectorAddition (TwoD a b) (ThreeD x y z) = error "Can't be different dimensions"

-- An operation to handle the multiplication of vectors to scalar constants
scalarMultiplication :: Float -> Vector -> Vector
scalarMultiplication a (TwoD x y) = TwoD (x*a) (y*a)
scalarMultiplication a (ThreeD x  y  z) = ThreeD (x*a) (y*a) (z*a)

-- An operation to get the length of a vector.
vectorLength :: Vector -> Float
vectorLength (TwoD x y) =  sqrt((x^2)+(y^2))
vectorLength (ThreeD x y z) = sqrt(x^2+y^2+z^2)  

-- An operation to get the distance between two vectors
vectorDistance :: Vector -> Vector -> Float 
vectorDistance (TwoD x y) (TwoD a b) = sqrt((x-a)^2+(y-b)^2)
vectorDistance (ThreeD x y z) (ThreeD a b c) = sqrt((x-a)^2+(y-b)^2+(z-c)^2)
vectorDistance (TwoD x y) (ThreeD a b c) =  error "Can't be different dimensions"
vectorDistance (ThreeD x y z) (TwoD a b) = error "Can't be different dimensions"


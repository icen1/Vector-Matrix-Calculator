module VectorOperations where

import System.Console.Haskeline
    ( defaultSettings, getInputLine, outputStrLn, runInputT, InputT )
import Text.Parsec
    ( char,
      digit,
      space,
      string,
      between,
      choice,
      many1,
      sepBy,
      parse,
      try,
      ParseError,
      Parsec )
import Data.Functor.Identity ()
import Data.List ()
import Data.Function ( on )

--------------------------------------MAIN IO-----------------------------------

-- The main IO function.
-- | This is where all the actual input takes place. I decided to use haskeline
-- | For the main IO function and loop and parsec to parse the command and the
-- | numbers given by the user.
main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           outputStrLn (unlines helpMenu)
           minput <- getInputLine "Write down the operation you want to use and the vectors after it: "
           case minput of
               Nothing -> pure ()
               Just "quit" -> pure ()
               Just input -> do let mainFunc = parserHelperCheck (parseErrorChecker $ parserHelper input)
                                    operation (Addition a b)      = vectorAddition a b
                                    -- operation (Multiplication a b) = scalarMultiplication a b
                                    -- operation (Length a) = vectorLength a
                                    -- operation (Dot a b)     = vectorDotProduct a b
                                    -- operation (Rotation a b) = vectorRotation a b 
                                    operation _ = error "Something wrong happened"
                                outputStrLn $ case mainFunc of
                                    (Distance a b) ->  show $ vectorDistance a b
                                    (Dot a b)      ->  show $ vectorDotProduct a b 
                                    sElse          ->  show $ operation mainFunc
                                loop

--------------------------- DATA TYPES USED ------------------------------------

-- Data type used to represent vectors. It can be 2D or 3D depening on the input
data Vector a
    =  TwoD a a
    |  ThreeD a a a
    deriving (Show, Read)

{-
    The data type that will be used to decide which vector function to call to 
    perform the operation required by the user. Also stores the elements/vectors
    that will be used in that operation
-}
data Mode a
        = Addition (Vector a) (Vector a)
        | Multiplication a (Vector a)
        | Length (Vector a)
        | Distance (Vector a) (Vector a)
        | Dot (Vector a) (Vector a)
        | Rotation a (Vector a)
        | NotValid
        deriving (Show,Read)

---------------------CONSTANTS THAT WILL BE MATCHED AGANIST---------------------

-- The operations that will be used and can be changed to the data type Mode
operations :: [String]
operations = ["Addition"
              ,"Multiplication"
              ,"Length"
              ,"Distance"
              ,"Dot"
              ,"Rotation"
              ,"Quit"
              ]

{- 
    The help menu that explains to the user how to format their operations and 
    what operations are avaliable.
-}
helpMenu :: [String]
helpMenu = 
    ["Write down the operation you want to use."
    ,"The descrption of the operation will be written on the right hand side of the '-'"
    ,"the command to use will be written on the left hand side of it \n"
    ,"Command         - Descrption \n"
    ,"Addition        - Adds two vectors to each other."
    ,"Multiplication  - Multiplies two vectors given"
    ,"Length          - Gets the length of the vector"
    ,"Distance        - Gets the distance between two vectors"
    ,"Dot             - Gets the dot product between two vectors"
    ,"Rotation        - Gets the roatation of a vector when given the angle. Must be in radian"
    ,"Quit            - To quit the program \n"
    ,"Write down the operation you want and then the vectors you want to input (only TwoD and ThreeD)"
    ,"They should be in the form 'Command (x y) (m n)' in case of 2D and 'Command (x y z) (m n l)' in 3D"
    ,"The operation and the numbers inputted will be parsed and the answer will be outputted\n"
    ]

-------------------------------PARSING COMMANDS---------------------------------

{- 
    The main parsing function. Uses the parse function imported from parsec and 
    also uses the function parseCommand to parse the operation given by the user
    and the vectors inputted 
-}
parserHelper :: String -> Either ParseError (Mode Double)
parserHelper = parse parseCommand []

{- 
    Checks whether a parse error was returned or a value. returns nothing if an 
    error occured and a just with the value if the value was correct.
-}
parseErrorChecker :: Either ParseError a -> Maybe a
parseErrorChecker (Right b) = Just b
parseErrorChecker (Left a) = Nothing

{- 
    Gets the value from the just and returns an error if it was a nothing
    as this means a parsing error occured.
-}
parserHelperCheck :: Maybe a -> a
parserHelperCheck (Just b) = b
parserHelperCheck Nothing = error "Couldn't parse the input probably"


{-
    The command where parsing mostly takes place. It maps the operation constant
    to the input and tries to match the string in the input to one that was in
    the operations list of strings, it then takes the vectors and give their
    output to parseCommandMap
-}
parseCommand :: Parsec String u (Mode Double)
parseCommand = do
        command <- choice $ map (try . string) operations
        space
        vecs <- parseVec `sepBy` space
        pure $ parseCommandMap command vecs

{-
    Parses the numbers given between brackets and separtes them by spaces. 
    Passes its output to vectorType to decide on which type of vector this is
    (either 2D or 3D)
-}
parseVec :: Parsec String u (Vector Double)
parseVec = do
        nums <- (between `on` char) '(' ')' matchVec
        pure $ vectorType nums
        where
            matchVec = many1 digit `sepBy` space

{-
    Gets the command strings and the vectors passed to it and maps them to a
    vector operation and returns it as a Mode.
-}
parseCommandMap :: (Floating a , Read a)  => String -> [Vector a] -> Mode a
parseCommandMap "Addition" [v1,v2] = Addition v1 v2
parseCommandMap "Multiplication" [v1] = Multiplication 5 v1
parseCommandMap "Length" [v1] = Length v1
parseCommandMap "Distance" [v1,v2] = Distance v1 v2
parseCommandMap "Dot" [v1,v2] = Dot v1 v2
parseCommandMap "Rotation" [v1] = Rotation (pi/2) v1
parseCommandMap _ _ = NotValid

{- 
    Decides on whether the vector is 2D or 3D and returns it with the numbers
    inputted by the user as doubles. 
-}
vectorType :: [String] -> Vector Double 
vectorType [x,y] = TwoD (read x) (read y)
vectorType [x,y,z] = ThreeD (read x) (read y) (read z)

-----------------------------VECTOR OPERATIONS----------------------------------

-- An operation to handle the addition of vectors to each other.
-- |
vectorAddition :: Vector Double -> Vector Double -> Vector Double
vectorAddition (TwoD x y) (TwoD a b) = TwoD (x+a) (y+b)
vectorAddition (ThreeD x y z) (ThreeD a b c) = ThreeD (x+a) (y+b) (z+c)
vectorAddition (ThreeD x y z) (TwoD a b) = error "Different dimensions"
vectorAddition (TwoD a b) (ThreeD x y z) = error "Different dimensions"

-- An operation to handle the multiplication of vectors to scalar constants
scalarMultiplication :: Double -> Vector Double -> Vector Double
scalarMultiplication a (TwoD x y) = TwoD (x*a) (y*a)
scalarMultiplication a (ThreeD x  y  z) = ThreeD (x*a) (y*a) (z*a)

-- An operation to get the length of a vector.
vectorLength :: Vector Double -> Double
vectorLength (TwoD x y) =  sqrt((x^2)+(y^2))
vectorLength (ThreeD x y z) = sqrt(x^2+y^2+z^2)

-- An operation to get the distance between two vectors
vectorDistance :: Vector Double -> Vector Double -> Double
vectorDistance (TwoD x y) (TwoD a b) = sqrt((x-a)^2+(y-b)^2)
vectorDistance (ThreeD x y z) (ThreeD a b c) = sqrt((x-a)^2+(y-b)^2+(z-c)^2)
vectorDistance (TwoD x y) (ThreeD a b c) =  error "Different dimensions"
vectorDistance (ThreeD x y z) (TwoD a b) = error "Different dimensions"

-- An Operation to get the dot product between two vectors
vectorDotProduct:: Vector Double -> Vector Double -> Double
vectorDotProduct (TwoD x y) (TwoD a b) = (x*a)+(y*b)
vectorDotProduct (ThreeD x y z) (ThreeD a b c) = (x*a)+(y*b)+(z*c)
vectorDotProduct (TwoD x y) (ThreeD a b c) =  error "Different dimensions"
vectorDotProduct (ThreeD x y z) (TwoD a b) = error "Different dimensions"

-- An operartion to rotate the vector
vectorRotation:: Double -> Vector Double -> Vector Double
vectorRotation a (TwoD x y) = TwoD (x*cos a - y*sin a) (x*sin a + y*cos a)
vectorRotation a (ThreeD x y z) = error "Will use matrices for that later"


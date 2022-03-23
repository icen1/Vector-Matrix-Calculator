module VectorOperations where

import System.Console.Haskeline
import Text.Parsec
import Data.Functor.Identity

main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           outputStrLn (unlines helpMenu)
           minput <- getInputLine "Write down the operation you want to use and the vectors after it:  "
           case minput of
               Nothing -> pure ()
               Just "quit" -> pure ()
               Just input -> do outputStrLn $ "Input was: " ++ input
                                loop

data Vector a
    =  TwoD a a
    |  ThreeD a a a
    deriving (Show, Read)

data Mode
        = Addition
        | Multiplication
        | Length
        | Distance
        | Dot
        | Rotation
        deriving (Show,Read)

-- mode :: StringParser Mode
-- mode = string "Addition" *> pure Addition
--        <|> string "Multiplication" *> pure Multiplication 
--        <|> string "Length" *> pure Length
--        <|> try (string "Distance") *> pure Distance 
--        <|> string "Dot" *> pure Dot
--        <|> string "Rotation" *> pure Rotation


operations :: [String]
operations = ["Addition"
              ,"Multiplication"
              ,"Length"
              ,"Distance"
              ,"Dot"
              ,"Rotation"
              ,"Quit"
              ]

helpMenu :: [String]
helpMenu = ["Write down the operation you want to use."
            ,"The descrption of the operation will be written on the right hand side of the '-'"
            ,"the command to use will be written on the left hand side of it \n"
            ,"Command         - Descrption \n"
            ,"Addition        - Adds two vectors to each other."
            ,"Multiplication  - Multiplies two vectors given"
            ,"Length          - Gets the length of the vector"
            ,"Distance        - Gets the distance between two vectors"
            ,"Dot     - Gets the dot product between two vectors"
            ,"Rotation        - Gets the roatation of a vector when given the angle. Must be in radian"
            ,"Quit            - To quit the program \n"
            ,"Write down the operation you want and then the vectors you want to input (only TwoD and ThreeD)"
            ,"The operation and the numbers inputted will be parsed and the answer will be outputted"
            ]

parserHelper :: String -> Either ParseError String
parserHelper = parse token []
                    where
                        matchVec = many1 digit `sepBy` space
                        token = do
                            command <- choice $ map (try . string) operations
                            spaces
                            firstVector <- between (char '(') (char ')') matchVec
                            spaces
                            secondVector <- between (char '(') (char ')') matchVec
                            pure $ command ++ concat firstVector ++  concat secondVector

parseCommand :: ParsecT String u Identity Mode
parseCommand = do
        command <- choice $ map (try . string) operations
        pure $ read command

parseVec :: ParsecT String u Identity (Vector a)
parseVec = do
        vector <- between (char '(') (char ')') matchVec
        pure $ read vector
        where
            matchVec = many1 digit `sepBy` space

            


-- parseParserHelper :: String -> Mode a
-- parseParserHelper = read(mode vector1 vector2) :: Mode 
--                         where
--                             s = parserHelper
--                             mode = many1 letter s
--                             vector1 = Vector (read (many1 digit `sepBy` space) :: Floating a)
--                             vector2 = Vector (read (many1 digit `sepBy` space) :: Floating )

-- An operation to handle the addition of vectors to each other.
vectorAddition :: Floating a => Vector a -> Vector a -> Maybe (Vector a)
vectorAddition (TwoD x y) (TwoD a b) = Just(TwoD (x+a) (y+b))
vectorAddition (ThreeD x y z) (ThreeD a b c) = Just(ThreeD (x+a) (y+b) (z+c))
vectorAddition (ThreeD x y z) (TwoD a b) = Nothing
vectorAddition (TwoD a b) (ThreeD x y z) = Nothing

-- An operation to handle the multiplication of vectors to scalar constants
scalarMultiplication :: Floating a => a -> Vector a -> Vector a
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
vectorDotProduct:: Floating a => Vector a -> Vector a -> Maybe a
vectorDotProduct (TwoD x y) (TwoD a b) = Just((x*a)+(y*b))
vectorDotProduct (ThreeD x y z) (ThreeD a b c) = Just((x*a)+(y*b)+(z*c))
vectorDotProduct (TwoD x y) (ThreeD a b c) =  Nothing
vectorDotProduct (ThreeD x y z) (TwoD a b) = Nothing

-- An operartion to rotate the vector
vectorRotation:: Floating a => a -> Vector a -> Vector a
vectorRotation a (TwoD x y) = TwoD (x*cos a - y*sin a) (x*sin a + y*cos a)
vectorRotation a (ThreeD x y z) = error "Will use matrices for that later"


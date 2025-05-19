import System.IO
import Data.List (intercalate)

data Person = Person {name :: String, age :: Int, female :: Bool}
    deriving (Show)

main :: IO()
main = do
    -- content <- readFile "proba.txt"
    -- print content

    -- appendFile "proba.txt" "\n!zdr"

    -- contents <- readFile "proba.txt"
    -- print (let fileLines = lines contents in fileLines)

    -- writeFile "out.txt" (serializePeople [Person {name = "A", age = 4, female = True}, Person {name = "Bbbbb", age = 5, female = False}])
    content <- readFile "out.txt"
    print $ deserializePeople (lines content)

validateNum :: Int -> Either String Int
validateNum n
    | n <= 0      = Left "Non-positive number"
    | n > 1000    = Left "Too large number"
    | True        = Right n

inputNumber :: Int -> Int
inputNumber n = case validateNum n of
    Right n -> n
    Left "Non-positive number" -> 1
    Left "Too large number" -> 999

-- Person -> String
-- [Person] -> String
-- String -> Person
-- String -> [Person]

serializePeople :: [Person] -> String
serializePeople = intercalate "\n" . map serializePerson
    where serializePerson (Person name age female) = name ++ " " ++ show age ++ " " ++ show female

deserializePeople :: [String] -> [Maybe Person]
deserializePeople = map strToPerson
    where strToPerson str = let parts = words str in if length parts /= 3 then Nothing else Just (Person (head parts) (read $ head $ tail parts) (if (last parts) == "True" then True else False))
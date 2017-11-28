module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "size"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits num = go num [] :: [Int]
    where
        go num digits
            | div num 10 == 0 = mod num 10 : digits
            | otherwise = go (div num 10) (mod num 10 : digits)

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits


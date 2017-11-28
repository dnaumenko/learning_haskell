module Database where

import Data.Time

data DatabaseItem = DbString String
                    | DbNumber Integer
                    | DbDate UTCTime

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
          (fromGregorian 1811 5 1)
          (secondsToDiffTime 34122))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
           (fromGregorian 1831 5 1)
           (secondsToDiffTime 34122))
  ]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr (\x y -> case x of
    DbDate x -> x : y
    _ -> y
  ) []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr (\x y -> case x of
    DbNumber x -> x : y
    _ -> y
  ) []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

stops  = "pbtdkg"
vowels = "aeiou"
stopVowelStop = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']




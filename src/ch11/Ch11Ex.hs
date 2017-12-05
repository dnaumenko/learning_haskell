module Ch11Ex where

import Data.Char


isSubseqOf :: (Eq a)
           => [a]
           -> [a]
           -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xxs@(x:xs) (y:ys)
  | x == y = isSubseqOf xs ys
  | otherwise = isSubseqOf xxs ys


capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\ x -> (x, capitalizeWord x)) . words

type Presses = Int
data Digit = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Zero | STAR | HASH deriving Show
type DaPhone = [(Char, (Digit, Int))]


phoneDict :: [(Digit, String)]
phoneDict = [(One, "")
                   , (Two, "abc")
                   , (Three, "def")
                   , (Four, "ghi")
                   , (Five, "jkl")
                   , (Six, "mno")
                   , (Seven, "pqrs")
                   , (Eight, "tuv")
                   , (Nine, "wxyz")
                   , (Zero, "+_")
                   , (HASH, ".,")
              ]

phone :: DaPhone
phone = concatMap (uncurry zipWithIndex) phoneDict where
  zipWithIndex digit string = zipWith (\char index -> (char, (digit, index))) string (enumToStrLen string)
  enumToStrLen s = enumFromTo 1 $ length s

reverseTaps :: DaPhone
            -> Char
            -> [(Digit, Int)]
reverseTaps [] _ = []
reverseTaps (x:xs) c
  | fst x == c = [snd x]

data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit a) = a
eval (Add e1 e2) = eval e1 + eval e2



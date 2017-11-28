module Ch9Ex where

import Debug.Trace
import Data.Char

eft :: (Ord a, Enum a) => a -> a -> [a]
eft f t = go f t []
    where go f t r
            | f > t = r
            | otherwise = go f (pred t) (t : r)

splitBySpace :: String -> [String]
splitBySpace s = splitBySeparator s ' '

splitByNewline :: String -> [String]
splitByNewline s = splitBySeparator s '\n'

splitBySeparator :: String -> Char -> [String]
splitBySeparator s sep = go s []
    where go [] res = reverse res
          go s res =
              trace ("calling go' with s = " ++ show s ++ " and res = " ++ show res)
              go (drop 1 . dropWhile isNotSeparator $ s) (takeWhile isNotSeparator s : res)
          isNotSeparator = (/= sep)

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

tuples = [(x, y) | x <- mySqr, y <- myCube]
tuplesFiltered = [(x, y) | (x, y) <- tuples, x < 50, y < 50]
tuplesFilteredSize = length tuplesFiltered

filterUpper xs  = [x | x <- xs, isUpper x]

capitalizeFirst (x: xs) = toUpper x : xs

capitalizeAll = map toUpper

filter3 = filter (\x -> mod x 3 == 0) [1..30]
filter3len = length . filter (\x -> mod x 3 == 0) $ [1..30]
filterArticles = filter (\x -> x /= "the" && x /= "a") . words $ "the brown dog was a goof"

zip' :: [a] -> [b] -> [(a, b)]
zip' [] xs = []
zip' xs [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

zipViaZipWith = zipWith (\ x y -> (x, y))

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs) = x || myOr xs


myAny :: (a -> Bool) -> [a] -> Bool
myAny pred a = myOr (map pred a)

myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny (== x)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish . map f $ xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy ordering (x:xs) = myMaximumBy' ordering x xs
  where
    myMaximumBy' :: (a -> a -> Ordering) -> a -> [a] -> a
    myMaximumBy' order m [] = m
    myMaximumBy' order m (a:as) =
        if order a m == GT then myMaximumBy' order a as else myMaximumBy' order m as
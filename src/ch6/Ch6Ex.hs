module Ch6Ex where

import Data.List

data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
    (==) (TisAn v) (TisAn v') = v == v'

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
    (==) (Two v t) (Two v' t') = v == v' && t == t'

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
    (==) (TisAnInt i) (TisAnInt i') = i == i'
    (==) (TisAString s) (TisAString s') = s == s'
    (==) _ _ = False

data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
    (==) (Pair a b) (Pair a' b') = a == a' && b == b'

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a = ThisOne a | ThatOne a
instance Eq a => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'
    (==) _ _ = False

data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)


data Mood = Blah | Woot deriving (Show, Eq)

settleDown x = if x == Woot
               then Blah
               else x

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

data Rocks = Rocks String deriving (Ord, Eq, Show) -- why both?
data Yeah = Yeah Bool deriving (Ord, Eq, Show)
data Papu = Papu Rocks Yeah deriving (Ord, Eq, Show)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

f :: RealFrac a => a
f = 1.0

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

myX = 1 :: Int
sigmund :: Int -> Int
sigmund x = myX

jung :: [Int] -> Int
jung xs = head (sort xs)

young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

-- signifier :: Ord a => [a] -> a
-- signifier xs = head (mySort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = (aToB a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith aToB i a = (aToB a) + fromInteger i

data Foo a = Foo a
f2 :: a -> a -> Foo Int
f2 a a' = Foo 1
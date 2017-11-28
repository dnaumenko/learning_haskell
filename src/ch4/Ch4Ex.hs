module Ch4Ex where

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x = if (x < 0) then -x else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

f1 :: (a, b) -> (c, d) -> ((b, d), (a, c))
f1 a b = ((snd a, snd b), (fst a, fst b))

-- we want a function that adds 1 to the length of a string argument and returns that result
x = (+)
f2 xs = x w 1 where w = length xs

---- id function
--x = x

-- return fst from tuple
f3 (a, b) = a
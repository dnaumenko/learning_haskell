module Ch7Ex where

f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
f (a, (b, d, (c, e)), c) (d, e, f) = ((a, e), (c, f))

functionC x y = case x > y of
    True -> x
    False -> y

ifEvenAdd2 n = case even n of
    True -> n + 2
    False -> n

nums x = case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

tensDigit :: Integral a => a -> a
tensDigit x = d
    where (xLast, _) = x `divMod` 10
          d          = xLast `mod` 10

tensDigit2 :: Integral a => a -> a
tensDigit2 x = d
    where (d, _) = x `divMod` 100

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b = case b of
    False -> x
    True  -> y

foldBool1 :: a -> a -> Bool -> a
foldBool1 x y b
    | b = y
    | otherwise = x

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f(a), c)
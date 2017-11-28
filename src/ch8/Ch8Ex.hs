module Ch8Ex where

sumOfFirstN :: (Eq a, Num a) => a -> a
sumOfFirstN num = go num 0
    where go n s
            | n == 0 = s
            | otherwise = go (n - 1) (s + n)

mul :: (Integral a) => a -> a -> a
mul a b = go a b 0
    where go a b product
            | b == 0 = product
            | otherwise = go a (b - 1) (product + a)

data DividedResult = Result Integer | DividedByZero deriving Show -- ? why I can't have a type constraint here?

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy num denom = go (abs num) (abs denom) 0 (signum num * signum denom)
    where go n d count sign
        | n < d = Result (toInteger $ count * sign)  // toInteger is the only option?
        | n == 0 = DividedByZero
        | otherwise = go (n - d) d (count + 1) sign

mc91 ::(Integral a) => a -> a
mc91 a
    | a > 100 = a - 10
    | a <= 100 = mc91 . mc91 $ a + 11
module LambdaFunctions where

addOneIfOdd n = case odd n of
    True -> (\x -> x + 1) n
    False -> n

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x
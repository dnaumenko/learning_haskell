module Ch3Ex where

ex1 :: String -> String
ex1 x = x ++ "!"

ex2 :: String -> Char
ex2 x = x !! 4

ex3 :: String -> String
ex3 x = drop 9 x


ex4 :: String -> Char
ex4 x = x !! 2

ex5 :: Int -> Char
ex5 x = "Curry is awesome!" !! x

ex6 :: String -> String
ex6 x = (drop 9 x) ++ " " ++ (take 2 (drop 6 x)) ++ " " ++ (take 5 x)
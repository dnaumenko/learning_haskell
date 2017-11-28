module Binds where


bindExp :: Integer -> String
bindExp x =
    let y = 5 in -- let experession bind y to value 5
    "the integer was: " ++ show x
    ++ " and y was: " ++ show y
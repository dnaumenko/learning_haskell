module Cipher where

import Data.Char

-- doesn't use mod by intention
caesar :: String -> String
caesar s = map (chr . (+3) . ord) "abc"
unCaesar s = map (chr . (-) 3 . ord) "abc"


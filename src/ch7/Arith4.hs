module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip1 :: (Show a, Read a) => a -> a
roundTrip1 = read . show

roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2 = read . show

main = do
    print (roundTrip2 4 :: Int)
    print (id 4)


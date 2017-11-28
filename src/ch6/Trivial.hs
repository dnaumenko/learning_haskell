module Trivial where

data Trivial = Trivial'

instance Eq Trivial where Trivial' == Trivial' = True

class Numberish a where
    fromNumber :: Integer -> a
    toNumber :: a -> Integer
    defaultNumber :: a
instance Numberish Age where
    fromNumber n = Age n
    toNumber (Age n) = n
    defaultNumber = Age 65

instance Numberish Year where
    fromNumber n = Year n toNumber (Year n) = n defaultNumber = Year 1988
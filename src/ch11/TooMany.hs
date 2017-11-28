{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module TooMany where


newtype Goats = Goats Int deriving (Eq, Show, TooMany)

--instance TooMany Goats where
--  tooMany (Goats n) = n > 43

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany n = fst n > 42

--instance TooMany (Int, Int) where
--  tooMany (a, b) = sum > 42 where sum = a + b

instance (Num a, TooMany a) => TooMany(a, a) where
  tooMany (a, b) = tooMany a && tooMany b
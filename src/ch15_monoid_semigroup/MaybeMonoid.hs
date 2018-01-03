module MaybeMonoid where

import Data.Monoid
import Control.Monad
import Test.QuickCheck

data Optional a = Nada | Only a deriving (Eq, Show)

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' (Only a)) _ = First' (Only a)
  mappend (First' Nada) b = b

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = firstGen

firstGen :: Arbitrary a => Gen (First' a)
firstGen = do
  a <- arbitrary
  frequency [ (1, return $ First' Nada)
            , (3, return $ First' (Only a)) ]

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId = First' String -> Bool


monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftId :: (Eq m, Monoid m) => m -> Bool
monoidLeftId m = (mempty <> m) == m

monoidRightId :: (Eq m, Monoid m) => m -> Bool
monoidRightId m = (m <> mempty) == m

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftId :: FstId)
  quickCheck (monoidRightId :: FstId)
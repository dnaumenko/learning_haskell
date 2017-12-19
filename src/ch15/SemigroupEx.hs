module SemigroupEx where

import Data.Semigroup
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
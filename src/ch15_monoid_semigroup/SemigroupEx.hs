module SemigroupEx where

import Data.Semigroup
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

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

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = threeGen

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

type ThreeAssoc = Three String String String -> Three String String String -> Three String String String -> Bool

---------------
newtype BoolConj = BoolConj Bool deriving Show

instance Semigroup BoolConj where
  (BoolConj b) <> (BoolConj b') = BoolConj (b && b')

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

newtype BoolDisj = BoolDisj Bool deriving Show

instance Semigroup BoolDisj where
  (BoolDisj b) <> (BoolDisj b') = BoolDisj (b || b')

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

---------------
data Or a b = Fst a | Snd b deriving Show
instance Semigroup (Or a b) where
  (Fst a) <> (Fst a') = Fst a'
  (Fst a) <> (Snd b) = Snd b
  (Snd b) <> (Fst a) = Snd b
  (Snd a) <> (Snd b) = Snd a

---------------
newtype Combine a b = Combine { unCombine :: a -> b }
instance Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \n -> f n S.<> f g

---------------
newtype Comp a = Comp {unComp :: a -> a}
instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp $ f . g

instance Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)

--------------

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
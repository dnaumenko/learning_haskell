module ChapterEx where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  ab <*> a = NopeDotJpg

instance Monad Nope where
  return _ = NopeDotJpg
  a >>= ab = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

--main = do
--  let trigger :: Nope (Int, String, Int)
--      trigger = undefined
--  quickBatch $ functor trigger
--  quickBatch $ applicative trigger
--  quickBatch $ monad trigger

-----------

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity ab) <*> (Identity a) = Identity (ab a)

instance Monad Identity where
  return = pure
  (Identity a) >>= ab = ab a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

--main = do
--  let trigger :: Identity (Int, String, Int)
--      trigger = undefined
--  quickBatch $ functor trigger
--  quickBatch $ applicative trigger
--  quickBatch $ monad trigger

-----------

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil ys = ys
  mappend (Cons x xs) ys = Cons x $ mappend xs ys

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons x xs) <*> ys = mappend (fmap x ys) (xs <*> ys)

instance Monad List where
  return = pure
  Nil >>= ab = Nil
  (Cons x xs) >>= ab = mappend (ab x) (xs >>= ab)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Cons a $ Cons b $ Nil

instance Eq a => EqProp (List a) where
  (=-=) = eq

main = do
  let trigger :: List (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

------------

j :: Monad m => m (m a) -> m a
j m = m >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 ab a = a >>= (return . ab)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 abc a b = abc <$> a <*> b

a' :: Monad m => m a -> m (a -> b) -> m b
a' ma mab = mab >>= (\f -> f <$> ma)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh a f =
module ListApl where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)

take' :: Int -> List a -> List a
take' n xs = take'' n xs Nil where
  take'' 0 xs result = result
  take'' n Nil result = result
  take'' n (Cons y ys) result = take'' (n - 1) ys (Cons y result)

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' f Nil _ = Nil
zipWith' f _ Nil = Nil
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) $ zipWith' f xs ys

repeat' :: x -> List x
repeat' x = Cons x $ repeat' x

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil ys = ys
  mappend (Cons x xs) ys = Cons x $ mappend xs ys

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Cons a (Cons b Nil)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Applicative List where
  pure a = Cons a Nil
  (<*>) (Cons x xs) a = mappend (fmap x a) (xs <*> a)
  (<*>) Nil _ = Nil

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs in take' 3000 l
          ys' = let (ZipList' l) = ys in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' $ repeat' a
  ZipList' xs <*> ZipList' ys = ZipList' $ zipWith' (\f x -> f x) xs ys

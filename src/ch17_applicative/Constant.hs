module Constant where

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant b) = Constant b

instance Monoid a => Applicative (Constant a) where
  pure b = Constant (mempty b)
  a <*> b = Constant (mappend (getConstant a) (getConstant b))
module ChEx where

data Constant a b = Constant b
instance Foldable (Constant a) where
  foldr f z (Constant b) = f b z

data Two a b = Two a b
instance Foldable (Two a) where
  foldr f z (Two a b) = f b z

data Three a b c = Three a b c
instance Foldable (Three a b) where
  foldr f z (Three a b c) = f c z

data Three' a b = Three' a b b
instance Foldable (Three' a) where
  foldMap f (Three' a b b') = mappend (f b) (f b')


filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF pred = foldr (\ el acc -> if pred el then mappend (pure el) acc else acc) mempty
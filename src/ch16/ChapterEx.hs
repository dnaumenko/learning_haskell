{-# LANGUAGE FlexibleInstances #-}

module ChapterEx where


data Sum b a = First a | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

data Company a c b = DeepBlue a c | Something b
instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a = L a b a | R b a b deriving (Eq, Show)
instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor a) = Bloor (f a)

data K a b = K a
instance Functor (K a) where
  fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K $ f a)

data EvilGoateeConst a b = GoatyConst b
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut g) = LiftItOut (fmap f g)
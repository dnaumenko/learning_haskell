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

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa f' g') = DaWrappa (fmap f f') (fmap f g')

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething f' g') = IgnoringSomething f' (fmap f g')

data List a = Nil | Cons a (List a)
instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a1 a2 a3) = MoreGoats (fmap f a1) (fmap f a2) (fmap f a3)

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
  fmap f Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g) = Read (f . g)
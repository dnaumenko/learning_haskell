module AssocQuickCheck where

import Data.Monoid
import Control.Monad
import Test.QuickCheck

asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
asc (<>) a b c = a <> (b <> c) == (a <> b) <> c

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftId :: (Eq m, Monoid m) => m -> Bool
monoidLeftId m = (mempty <> m) == m

monoidRightId :: (Eq m, Monoid m) => m -> Bool
monoidRightId m = (m <> mempty) == m

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools)
                        , (1, return Twoo)]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

main :: IO ()
main = do
  let ma = monoidAssoc
      mli = monoidLeftId
      mri = monoidRightId
  quickCheck (ma :: BullMappend)
  quickCheck (mli :: Bull -> Bool)
  quickCheck (mri :: Bull -> Bool)
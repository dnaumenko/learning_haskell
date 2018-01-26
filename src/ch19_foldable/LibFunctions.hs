module LibFunctions where

sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' e = foldr (\ el acc -> acc || el == e ) False

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr (min . Just) Nothing

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (max . Just) Nothing

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ acc -> acc + 1) 0 -- \x acc -> acc + 1 <=> const (1+)

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap (mappend mempty)

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x acc -> mappend acc (f x)) mempty
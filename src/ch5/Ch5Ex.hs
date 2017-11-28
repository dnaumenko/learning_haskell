module Ch5Ex where

r :: [a] -> [a]
r [x] = [x]
r x = take 0 x

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = (bToC (aToB a))

a :: (a -> c) -> a -> a
a _ b = b

a' :: (a -> b) -> a -> b
a' aToB a = aToB a

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x > y) then fstString x else sndString y
     where x = "Singin"
           y = "Somewhere"


f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h i = g (f i)


data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e a = w (q a)


data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)


munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
munge xToY yToWZ x = fst (yToWZ (xToY x))

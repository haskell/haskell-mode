-- Comments are coloured brightly and stand out clearly.

repeat :: a -> [a]
repeat xs = xs where xs = x:xs          -- Keywords are also bright.

head :: [a] -> a
head (x:_) = x
head [] = error "PreludeList.head: empty list" -- Strings are coloured softly.

data Maybe a = Nothing | Just a              -- Type constructors, data
             deriving (Eq, Ord, Read, Show)  -- constructors, class names
                                             -- and module names are coloured
                                             -- closer to ordinary code.

recognize +++ infix :: Operator Declarations
as `well` as = This Form
(+) and this one = as well

instance Show Toto where
    fun1 arg1 = foo             -- FIXME: `fun1' should be highlighted.

constStr = "hello \
           \asdgfasgf\
           \asf"

{-
map :: (a -> b) -> [a] -> [b]           -- Commenting out large sections of
map f []     = []                       -- code can be misleading.  Coloured
map f (x:xs) = f x : map f xs           -- comments reveal unused definitions.
-}

-- Note: the least significant bit is the first element of the list
bdigits               :: Int -> [Int]
bdigits 0             = [0]
bdigits 1             = [1]
bdigits n | n>1       = n `mod` 2 :

-- arch-tag: a0d08cc2-4a81-4139-93bc-b3c6be0b5fb2

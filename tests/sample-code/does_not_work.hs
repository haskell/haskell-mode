main = interact $ show . myfib . readInt

readInt :: String -> Integer
readInt = read

myfib :: Integer -> Integer
myfib n = (fibs) !! n

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

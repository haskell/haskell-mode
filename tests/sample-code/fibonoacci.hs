main = interact $ show . myfib . readInt

readInt :: String -> Int
readInt = read

myfib :: Int -> Integer
myfib n = (fibs) !! n

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- compute the list of binary digits corresponding to an integer
-- Note: the least significant bit is the first element of the list
bdigits               :: Int -> [Int]
bdigits 0             = [0]
bdigits 1             = [1]
bdigits n | n>1       = n `mod` 2 :
                        bdigits (n `div` 2)
          | otherwise = error "bdigits of a negative number"

--  compute the value of an integer given its list of binary digits
--  Note: the least significant bit is the first element of the list
bvalue :: [Int]->Int
bvalue [] = error "bvalue of []"
bvalue s  = bval 1 s
            where
            bval e []                    = 0
            bval e (b:bs) | b==0 || b==1 = b*e + bval (2*e) bs
                          | otherwise    = error "illegal digit"

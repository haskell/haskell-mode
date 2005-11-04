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
bvalue s  = bval 1 s            -- FIXME
    where
      bval e [] = 0           -- FIXME: indentation below `where'.
      bval e (b:bs) | b==0 || b=="dd of " = b*e + bval (2*e) bs
                    | otherwise    = error "illegal digit" -- Spurious 3rd step.
                                     foo

-- FIXME: tab on the line above should insert `bvalue' at some point.

{- text
   indentation
   inside comments
 -}
toto a = ( hello
         , there                -- indentation of leading , and ;
         -- FIXME: indentation of this comment.
         , my friends )

lili x = do let x = 1
            print x     -- FIXME: align with `let'

titi b =
    do expr1
       {- text
        - indentation
        - inside comments
        -}
       let foo   = let fro = 1
                       fri = 2   -- FIXME: can't indent lower than `let'.
                   in
                     hello      -- FIXME
           foo2 = bar2  -- FIXME: can't align with empty arg in foo.
       expr2            -- FIXME: two possible `let's.

tata c =
    let bar = case foo
              of 1 -> blabla
                 2 -> blibli    -- FIXME: only one possible indentation here.
        bar = case foo of
                _ -> blabla     -- FIXME: indentation below `case'.
        bar' = case foo
               of _ -> blabla
                  toto -> plulu
    
turlu d = if test               -- FIXME: indentation below if
          then
              ifturl               -- FIXME: indentation below then
          else
             adfaf             -- FIXME

turlu d = if test then
             ifturl               -- FIXME: indentation below then
          else
              sg                -- FIXME

turly fg = toto
    where
      hello = 2
           

-- test from John Goerzen

x myVariableThing = case myVariablething of
                      Just z -> z
                      Nothing -> 0

foo = let x = 1 in toto
              titi              -- FIXME

foo = let foo x y = toto
              where
                toto =
      if blabla then

instance Show Toto where
    foo x 4 = 50
         
data Toto = Foo
          | Bar
            deriving (Show)     -- FIXME

    eval env (Llambda x e) =    -- FIXME: sole indentation?
        Vfun (\v -> eval (\y -> if (x == y) then v else env y)
                     e) -- FIXME

foo = case findprop attr props of
                               Just x -> x -- FIXME: useless option


       data T = T { granularity :: (Int, Int, Int, Int) -- FIXME: sole indentation?
           , items :: Map (Int, Int, Int, Int) [Item] }

-- arch-tag: de0069e3-c0a0-495c-b441-d4ff6e0509b1

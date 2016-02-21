module A where

data A = A { a :: Char }

fn :: A -> A
fn A{..} = undefined

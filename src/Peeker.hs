module Peeker where

type Peek a b = a -> (b, b -> a)

(~>) :: Peek a b -> Peek b c -> Peek a c
(~>) p1 p2 x = (get p2 y, upd p1 x . upd p2 y)
               where y = get p1 x 

get :: Peek a b -> a -> b
get p = fst . p

upd :: Peek a b -> a -> b -> a
upd p = snd . p

liftP :: Peek a a
liftP x = (x, id)

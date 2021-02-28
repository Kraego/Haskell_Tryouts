-- param 1 apply a return bool - predicate (stop case for creating list)
-- param 2 function transform (a to b) applied on x for lists new head
-- param 3 function applied to x and used for recursion
-- param 4 the value x to work with
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
  | p x       = []
  | otherwise = h x : unfold p h t (t x)

-- when x == 0 stop
int2bin :: Integer -> [Integer]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

{--
  Run Sequence for 5

  int2bin 5
    otherwise: 5 mod 2 : unfold (==0) ('mod' 2) ('div' 2) (5 'div' 2)
                  |                     |
                  v                     v
                  1     : unfold (==0) ('mod' 2) ('div' 2) 2
                              otherwise: 2 mod 2 : unfold (==0) ('mod' 2) ('div' 2) (2 'div' 2)
                                          |                         |
                                          v                         v
                                          0 : unfold (==0) ('mod' 2) ('div' 2) 1
                                                otherwise: 1 mod 2 : unfold (==0) ('mod' 2) ('div' 2) (1 'div' 2)
                                                              |                     |
                                                              v                     v
                                                              1   : unfold (==0) ('mod' 2) ('div' 2) 0
                                                                  p x case - End
Result [1, 0, 1]
-}                                          

-- param 1 stop on empty list
-- param 2 apply f to head of xs
-- param 3 use tail of xs
-- param 4 xs applied via currying
mapU :: (a->b) -> [a] -> [b]
mapU f = unfold null (f . head) tail

-- p is a lambda function anonymous function: allways returning false
iterateU :: (a -> a) -> a -> [a]
iterateU = unfold (const False) id

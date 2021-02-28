data Nat = Zero | Succ Nat deriving Show

sub1 :: Nat -> Nat -> Nat
sub1 n Zero = n
sub1 (Succ n) (Succ m) = sub1 n m

add1 :: Nat -> Nat -> Nat
add1 n Zero = n
add1 (Succ n) (Succ m) = add1(Succ(Succ n)) m 

{-- 

Division thoughts:

6 : 2 = 3

cnt = 0

cause 6-3 = 3  cnt++
      3-3 = 0  cnt++
      0-3 = right Operand is 0 return count
-}

-- Count possible subs
_div :: Nat -> Nat -> Nat -> Nat
_div Zero _ x = x
_div n m x = _div (sub1 n m) m (Succ x)

div1 :: Nat -> Nat -> Nat
div1 n (Succ Zero) = n
div1 n m = div1 (_div n m Zero) (Succ Zero) 


-- For testing
two = Succ(Succ Zero)
one = Succ Zero

six = Succ(Succ(Succ(Succ(Succ(Succ Zero)))))

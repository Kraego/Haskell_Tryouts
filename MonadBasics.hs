{-# LANGUAGE InstanceSigs #-}

import Data.Foldable


liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = 
    m >>= \a -> 
    return (f a)

{--
Example: 
> liftM (+1) (Just 0)
Just 1

-}   

-- same principle with continuation
liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m1 m2 = 
    m1 >>= \a -> 
    m2 >>= \b ->
    return (f a b)
-- Example liftM2 (,) (Just 3) (Just 5) = Just (3,5)

{--
Write a Monad instance for newtype Identity a = Id a. You need to provide an 
implementation for Functor and Applicative as well! An example usage is 
liftM2 (+) (Id 10) (Id 11) = Id 21. Show that the 3 monadic laws hold for 
your implementation.
-}
newtype Identity a = Id a deriving Show

instance Functor Identity where
  fmap f (Id a) = Id (f a)

-- fmap (*2) (Id 5)

instance Applicative Identity where
  pure x = Id x
  (<*>) (Id x) (Id y) = Id (x y)

-- pure (+) <*> Id 5 <*> Id 10 

instance Monad Identity where
  return :: a -> Identity a
  return = pure

  (>>=) :: Identity a -> (a -> Identity b) -> Identity b
  (Id a) >>= f = f a

{--
Check 3 equational laws:
1. return x >>= f = f x

2. mx >>= return = mx

3. (mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))
-}


testFunc1 :: Int -> Identity [Char]
testFunc1 x = Id (replicate x 'x')

first1 = return 5 >>= testFunc1
first2 = testFunc1 5

second1 = first1 >>= return

testFunc2 :: [Char] -> Identity [Char]
testFunc2 x = Id (x)

third1 = (first1 >>= testFunc2) >>= testFunc2
third2 = first1 >>= (\x -> (testFunc2 x >>= testFunc2))


{--
Implement mapM and mapM_ assuming the Traversable to be a list, 
therefore the types change to: mapM :: Monad m => (a -> m b) -> [a] -> m [b] 
and mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
-}

{--
Other Solution:

mapMList :: Monad m => (a -> m b) -> [a] -> m [b] 
mapMList _ []     = return []
mapMList f (a:as) = f a >>= \b -> 
                    mapMList f as >>= \bs -> 
                    return (b:bs)
-}

mapList :: Monad m => (a -> m b) -> [a] -> m [b]
mapList _ [] = return []
mapList f (a:as) = liftM2 (++) (f a >>= \b -> return [b]) (mapList f as)

maptry = mapList testFunc1 [1,2,3,4] -- should be ['x', 'xx', 'xxx', 'xxxx']

-- hier Ergebnis verwerfen
mapList_ :: Monad m => (a -> m b) -> [a] -> m ()
mapList_ _ [] = return ()
mapList_ f (a:as) = f a >>
                    mapList_ f as >>
                    return ()

{--
Implement mapM_ but now with the original type 
(HINT: Foldable is the key): mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
-}

-- extract a from t a then apply monadic action with discard >>
mymapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mymapM_ f t = mapList_ f $ toList t

-- Implement sequence_ :: (Foldable t, Monad m) => t (m a) -> m ().
mysequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
mysequence_ t = foldr (>>) (return ()) t

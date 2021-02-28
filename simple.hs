med :: Fractional a => [a] -> a
med xs 
    | null xs = 0
    | odd (length xs) = xs !! (length xs `div` 2)
    | otherwise = (half1 + half2) / 2
   where 
       half1 = xs !! ((length xs `div` 2) - 1) 
       half2 = xs !! (length xs `div` 2)

sumdown :: Int -> Int
sumdown x
    | x == 0 = 0
    | otherwise = x + sumdown (x - 1)

fib :: Int -> Int
fib x
    | x == 0 = 0
    | x == 1 = 1
    | otherwise = fib (x-1) + fib (x-2)

euclid :: Int -> Int -> Int
euclid x y
    | y == 0 = x
    | otherwise = euclid y (x `mod` y)

pow :: Int -> Int -> Int
pow 0 _ = 0
pow _ 0 = 1
pow x 1 = x
pow x y = x * pow x (y-1)

compress :: Eq a => [a] -> [a]
compress (x:xs)
    | null xs = [x] -- is last element
    | x == head xs = compress(x:tail xs) -- skip: same
    | otherwise = x:compress xs -- append 

isSubGroup :: Eq a => a -> [[a]] -> Bool 
isSubGroup y (x:xs)
    | y == head x = True -- if match return true
    | null xs = False    -- at end of list - not found - false
    | otherwise = isSubGroup y xs --recursion

addToSub :: Eq a => a -> [[a]] -> [[a]]
addToSub y (x:xs)
    | y == head x = (x++[y]):xs   -- add to existing
    | otherwise = x:addToSub y xs -- recursion thru input

-- tailrecursive - speeeeeed
pack :: Eq a => [a] -> [[a]]
pack (x:xs)
   | null xs = [[x]]
   | x `isSubGroup` packed = addToSub x packed
   | otherwise = [x]:packed
    where
        packed = pack xs 


encode :: Eq a => [a] -> [(Int, a)]
encode xs = encodePacked $ pack xs

encodePacked :: [[a]] -> [(Int, a)]
encodePacked (x:xs)
    | null xs = [(length x, head x)]
    | otherwise = (length x, head x):encodePacked xs

-- Testdata for pack and encode
foo = [9,1,2,1,2,2,2,3,3,3,4,4,1,1,1,2,2,2,3,3,1,1,8,8,9]
foo1 = ['a', 'a', 'b', 'c', 'c', 'b', 'a']
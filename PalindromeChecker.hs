import Data.Char (toLower)

normalize :: [Char] -> [Char]
normalize xs = filter (/=' ') $map toLower xs

palindrome :: [Char] -> Bool
palindrome xs = reverse ns == ns
    where 
        ns = normalize xs

{- Tryout 
    *Main> palindrome "A man a plan a canal Panama"
    True
    *Main> palindrome "A man a plan a canal Panama1"
    False
-}
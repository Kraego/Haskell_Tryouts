<!-- omit in toc -->
# Kraegi Haskell Cheatsheet 

- [If working with stack](#if-working-with-stack)
- [Familiar Functions / Operators](#familiar-functions--operators)
- [Functions on Lists](#functions-on-lists)
- [Function Application (Zuweisung)](#function-application-zuweisung)
- [Syntax](#syntax)
- [GHCi (Compiler interpreter)](#ghci-compiler-interpreter)
  - [Add packages to ghci](#add-packages-to-ghci)
- [Types](#types)
- [Conditions](#conditions)
- [**$\lambda$ Lambda Expressions**](#lambda-lambda-expressions)
- [Currying](#currying)
- [Recursions](#recursions)
- [Folds](#folds)
- [Maybe](#maybe)
- [Functor](#functor)
- [Randomness](#randomness)
- [Working with Monads](#working-with-monads)
  - [IO Monad](#io-monad)
- [Concurrency](#concurrency)
  - [Lock Based](#lock-based)
  - [Lockfree Coding](#lockfree-coding)
  - [Parallelism](#parallelism)

## If working with stack

1. stack init ... Initialize Project
2. stack install packages needed
3. Set vs code Plugin to isolated ghc from stack project 
  * f.e.:C:\sr\snapshots\ab07bc89\lib\x86_64-windows-ghc-8.10.3

## Familiar Functions / Operators

* \+
* \*

## Functions on Lists

* **Insert at head (list xs):** ```newelement : xs```
* **Append at end (list xs):** ```xs ++ [newElement]```
* **Join two lists:** ```list1 ++ list2```
* First: ```head [1,2,3,4,5]``` -> 1
* Remove first: ```tail [1,2,3,4,5]``` -> [2,3,4,5]
* Select n<sup>th</sup> element of list: ```[1,2,3,4,5] !! 2``` -> 3
* Select first n elements: ```take 3 [1,2,3,4,5]``` -> [1,2,3]
* Remove first n elements: ```drop 3 [1,2,3,4,5]``` ->[4,5]
* Length: length
* Sum: sum
* Product: product
* Append lists ```[1,2,3] ++ [4,5]``` -> [1,2,3,4,5]
* Reverse a list ```reverse [1,2,3,4,5]``` -> [5,4,3,2,1]

## Function Application (Zuweisung)

* Syntax: **with space character**
  * Example:
    * Mathematically: f(a,b) + c d
    * Haskell: f a b + c*d
* *Function Application higher priority than any other operator!*

## Syntax 

* average ns = sum ns \`div` length ns (back quotes)
* x ‘f‘ y is just syntactic sugar for f x y (nicer to read)
  * example div takes to parameters
    * div 5 3 ist the same as 5 \`div` 3
* **Don't use parentheses not idiomatic**
``` haskell
putStrLn (show (1 + 1))
putStrLn $ show $ 1 + 1 --better
```
* **Functions and arguments must begin with lower case**
* Convention: lists has s as suffix (f.e.: xs)
* **Grouping with Spaces** (instead of brackets)
* Comments:
  * Single line with *-- *
  * Multiline with *{- \[comment] -}*
* **!** for force evaluation (*deactivate laziness*)

## GHCi (Compiler interpreter)

* start with *ghci* \<optional startscript *.hs> on Powershell
* to reload script (after change/save): *:reload* or *:r* (shorthand)
* get type of function: *:t \<function>*
* get infos about type especially monad stuff **:i \<function>** (f.e.: :i (>==))
* exit with *:quit*, *:q* (shorthand)
* **kind** - "type of type" *:k*

### Add packages to ghci

With **stack** packages can be added  for example random
  * *stack ghci --package random*

## Types

  * check get type with (partly describe the function):
    * *:type* \[value] (or :t \[value])
    * f.e.: type False --> bool
  * **Lists**
    * Declaration Example \[False, True, False] :: \[Bool]
    * Lists with different number of Values are different types
    * List of Lists: \[\['a], \['a', 'b']] :: \[\[char]]
    * can just contain on type (restricted types)
  * **Tuples**
    * (t1, t2, ... tn)
    * f.e.: (False, True) :: (Bool,Bool)
    * Type defined by tuple size
    * **types are unrestricted** (f.e.: ('a', (true, false)) :: (char, (Bool,Bool)))
  * **Function Types**
    * Result types are unrestricted
    * Example:
    ``` haskell
      add :: (Int,Int) -> Int
      add (x,y) = x + y
    ```
    * **Curried Functions** (returning function as result) with multiple arguments:
    ``` haskell
     add' :: Int -> (Int -> Int)
     add' x y = x + y
    ```
      * **should be preferred** (more flexible)
      * It's how you do dependency injection in functional programming
      * partially applying arguments
  * **Type declaration**
    * create aliases - **type** keyword
      * Example: *type String = \[Char\]*
      * Can be parametrized:
        * *type Pair a = (a,a)*
      * Can be nested but **not recursive** (like tree)
    * If **compiler should distinguish new type**:
      * *newtype Pos = P (Int, Int)*
        * *P (Int, Int)* ... Data Constructor
    * Complete new type - **data declaration**
      * Example: *data Bool = False | True*
        * Bool ... typeconstructor
        * False | True ... dataconstructor
      * Has to start with uppercase letter
      * Can have parameters: *data Maybe a = Nothing | Just a*
        * Just always needs a type
        * avoid checking everything against null (like in Java)
        * If a function returns *Maybe [Int]* you have to check it
      * **These types can be recursive!**
        * *Nat* - isomorphic to Int
  * **Implement types:**
    * Example: Eq
``` haskell
instance Eq Day where
  Monday == Monday  = True
  ...
  _      == _       = False 
```

## Conditions
* f.e.:
  ``` haskell
    signum n = if n > 0 then -1 else 
                if n == 0 then 0 else 1
  ```
* if else statements must be **complete** (always must have else)
* **Guarded equations**
  * f.e.:     
  ``` haskell
  signum n | n < 0      = -1
           | n == 0     = 0
           | otherwise  = 1
  ```
    * otherwise is defined True
* **Pattern matching** define a Function with possible input chases -> output
  * or output chases (example and &&)
  * You have to define every possible pattern (else runtime exception)
  * **Patterns are check from top to bottom!** (can be useful)
  * _ wildcard used in pattern definitions
  * List Patterns
    * example:
  ``` haskell
     head :: [a] -> a
     head (x : _) = x
  ```
  * If something gets calculated multiple times use **where** to calculate it one time.
  ``` haskell
  bmiTell :: (RealFloat a) => a -> a -> String  
  bmiTell weight height  
      | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
      | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
      | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
      | otherwise                   = "You're a whale, congratulations!" 

  -- becomes this

  bmiTell :: (RealFloat a) => a -> a -> String  
  bmiTell weight height  
      | bmi <= skinny = "You're underweight, you emo, you!"  
      | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
      | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
      | otherwise     = "You're a whale, congratulations!"  
      where bmi = weight / height ^ 2  
            skinny = 18.5  
            normal = 25.0  
            fat = 30.0  
  ```

## **$\lambda$ Lambda Expressions**
  * anonymous Functions
  * Usage: For **higher-order functions**
    * Many Haskell functions are "higher-order functions", i.e., they expect other functions as parameters. Often, the functions we want to pass to such a higher-order function are used only once in the program, at that particular point. It's simply more convenient then to use a lambda expression than to define a new local function for that purpose.
  * Syntax:
  ``` haskell
     \x -> x + x
  ```
  * **Example:** Here's an example that filters all even numbers that are greater than ten from a given list:
  ``` haskell
  ghci> map (\ x -> x^2 + x) [1..10]
  [2,6,12,20,30,42,56,72,90,110]
  ```

## Currying

* Curried Function == Higher Order Function
* Since haskell functions just accept one or none param, you could either use a tuple or currying, for every value in tuple a function is defined, as long as not all arguments are passed a function is returned which calculates part of the result.
* Putting a **space between two things is simply function application**. The space is sort of like an operator and it has the highest precedence.
* So how is that beneficial to us? Simply speaking, if we call a function with too few parameters, we get back a partially applied function, meaning a function that takes as many parameters as we left out. Using partial application (calling functions with too few parameters, if you will) is a neat way to create functions on the fly
  * Example:    
``` haskell 
    multThree :: (Num a) => a -> a -> a -> a  
    multThree x y z = x * y * z  

    ghci> let multTwoWithNine = multThree 9  
    ghci> multTwoWithNine 2 3  
    54  
    ghci> let multWithEighteen = multTwoWithNine 2  
    ghci> multWithEighteen 10  
    180  
```
* Example:
``` haskell
uncurried_add (x, y) = x + y
curried_add x = \y -> x + y
-- the same as above but with syntactical sugar
curried_add x y = x + y
```

## Recursions

* Done by pattern matching:
  * **Function constraint** is a must
    * f.e.: define that it's a list of Numbers
      * (product :: Num a => \[a\] -> a)
      * of Ord a => \[a\] -> \[a\]
        * if you need compare f.e.: a <= x, ...
      * It's like an interface
  * **base case** (must be on top) ... stop condition for recursion
  * recursive case (input must getting structurally smaller)
* Measure Performance with *:set +s*

## Folds

* apply function to each element of a structure
* Syntax: 
    * foldr (\[function\]) \[Basecase\]
    * Example: sum = foldr (+) 0
* *foldr* and *foldl*
* foldl' ... strict version of foldl (\['\] is called prime)

## Maybe

* Usage: find :: (a -> Bool) -> [a] -> Maybe a

## Functor

* *fmap* ... functorial map implement it

## Randomness

* To use random **start ghci with stack**: *stack ghci*
* Import Random: 
  * import System.Random
  * let g = mkStdGen 42

## Working with Monads

* We used the then operator (*>>*, continuation) and the bind operator (*>>=*) for Programming with side effects. See sum of three rolls, this is not the real way to do it
  * use **do Notation** instead -> more readable (return is **pure** from applicative, most of the time - u can say its the default implementation)
``` haskell
-- see Chapter12 
sumOfThreeRolls :: (Int, Int) -> State StdGen Int
sumOfThreeRolls r =
  randomRangeState r >>=
    \x -> randomRangeState r >>=
      \y -> randomRangeState r >>=
        \z -> return (x+y+z)
-- versus
sumOfThreeRolls :: (Int, Int) -> State StdGen Int
sumOfThreeRolls r = do
  x <- randomRangeState r
  y <- randomRangeState r
  z <- randomRangeState r
  return (x+y+z)
```

### IO Monad

* There is stuff for fileaccess, console access, random, Clock, FFC like native function call in java - call C-Stuff (everything with sideeffects) -> tip separate that stuff from pure functions
* Monads are pure - but IO Monad has sideffects (not under controll of the programm)
* IO Monad Functions are called actions, f.e.
``` haskell
play :: String -> IO ()
play word = do
    putStr "? "
    guess <- getLine
    if guess == word then
        putStrLn "You got it!"
    else do
        putStrLn (match word guess)
        play word
```
* There are even mutable references (like pointers in C)

## Concurrency

### Lock Based

* if just one reference is needed use **IORef**
* use **MVar** -> 
  * Hold value and works like a binary sempahore (no delay needed)
  * Or as channel (takeMVar is receive, putMVar is send)
  * Or as synchronized mutable Variable for all Threads
* async/await also possible - https://hackage.haskell.org/package/async-2.2.2

### Lockfree Coding
* see: http://hackage.haskell.org/package/stm-2.5.0.0/docs/Control-Concurrent-STM-TMVar.html

### Parallelism

* for pure computations (cause they are side effect free)
* two flavours:
  * **Evaluation** - lazy types parallel evaluate (f.e.: Lists)
  * **Data-Flow** - Par Monad - explicit Model for parallel data-flows


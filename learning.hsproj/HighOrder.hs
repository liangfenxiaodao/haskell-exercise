module HighOrder where
  
zipWith':: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y: zipWith' f xs ys
--
-- As you can see, a single higher order function can be used in very versatile ways.
-- Imperative programming usually uses stuff like for loops, while loops, setting something to a variable, checking its state, etc.
-- to achieve some behavior and then wrap it around an interface, like a function.
-- Functional programming uses higher order functions to abstract away common patterns,
-- like examining two lists in pairs and doing something with those pairs
-- or getting a set of solutions and eliminating the ones you don't need.

map'::(a->b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter'::(a->Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x       = x : filter f xs
    | otherwise = filter f xs

-- Mapping and filtering is the bread and butter of every functional programmer's toolbox
large :: (Integral a) => a
large = head (filter p [100000, 99999 ..])
      where p x = x `mod` 3889 == 0

-- find the sum of all odd squares that are smaller than 10,000.
result = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

collatzArray::(Integral a) => a -> [a]
collatzArray 1 = [1]
collatzArray n
    | odd n  = n:collatzArray(3*n + 1)
    | even n = n:collatzArray(n `div` 2)

-- with the help of $, which means function application, we can not only get rid of parentheses, but also treat $ just like another function.
-- That way, we can map function application over a list of functions
-- map ($ 3) [(4+), (10*), (^2), sqrt]

-- function composition can be used to simplify code, like

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- can be rewritten to

oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

-- However, if there was a chance of someone else reading that code, I would have written it like this:
--
-- oddSquareSum :: Integer
-- oddSquareSum =
--     let oddSquares = filter odd $ map (^2) [1..]
--         belowLimit = takeWhile (<10000) oddSquares
--     in  sum belowLimit

-- find out how many times each element appears in the list.
-- map (\list@(x:xs) -> (x,length list)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]

respondPalindromes = unlines . map (\xs -> if isPalindromes xs then "palindrome" else "not a palindrome") . lines
                        where isPalindromes xs = xs == reverse xs
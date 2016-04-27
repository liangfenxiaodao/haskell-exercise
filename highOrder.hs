
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

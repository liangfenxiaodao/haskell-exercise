module Fold where

-- We'd introduce the x:xs pattern and then we'd do some action that involves a single element and the rest of the list.
-- It turns out this is a very common pattern, so a couple of very useful functions were introduced to encapsulate it.
-- These functions are called folds.

-- A fold takes a binary function, a starting value (I like to call it the accumulator) and a list to fold up.
-- The binary function itself takes two parameters.
-- The binary function is called with the accumulator and the first (or last) element and produces a new accumulator.
-- Then, the binary function is called again with the new accumulator and the now new first (or last) element, and so on.
-- Once we've walked over the whole list, only the accumulator remains, which is what we've reduced the list to.

-- with Fold, this implementation can be simplified to

ele ::(Eq a) => a -> [a] -> Bool
ele a [] = False
ele a (x:xs)
  | a == x = True
  | otherwise = a `ele` xs

ele' ::(Eq a) => a -> [a] -> Bool
ele' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- and map function can be implemented like

map'::(a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
-- we could have implemented this function with a left fold, but it's more complicated that foldr:
-- map' f xs = foldl (\acc x -> acc ++ [f x]) [] xs
-- because (f x) is not a list, so we cannot write the function like this:
-- map' f xs = foldl (\acc x -> acc : f x) [] xs
-- otherwise we will get following error:

-- fold.hs:27:38:
--     Couldn't match expected type ‘[b]’ with actual type ‘b’
--       ‘b’ is a rigid type variable bound by
--           the type signature for map' :: (a -> b) -> [a] -> [b]
--           at fold.hs:26:7
--     Relevant bindings include
--       acc :: [b] (bound at fold.hs:27:21)
--       f :: a -> b (bound at fold.hs:27:6)
--       map' :: (a -> b) -> [a] -> [b] (bound at fold.hs:27:1)
--     In the second argument of ‘(++)’, namely ‘(f x)’
--     In the expression: acc ++ (f x)

-- Note:

-- Folds can be used to implement any function where you traverse a list once, element by element,
-- and then return something based on that.
-- Whenever you want to traverse a list to return something, chances are you want a fold.
-- That's why folds are, along with maps and filters, one of the most useful types of functions in functional programming.

-- Now let's rewrite some functions with fold

max' :: (Ord a) => [a] -> a
max' = foldl1 (\acc x -> if acc > x then acc else x)
-- or
-- max' xs = foldr1 (\x acc -> if acc > x then acc else x) xs

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldl (\acc x -> if f x then x : acc else acc) []

-- Another way to picture right and left folds is like this: 
-- say we have a right fold and the binary function is f and the starting value is z.
-- If we're right folding over the list [3,4,5,6], we're essentially doing this: f 3 (f 4 (f 5 (f 6 z))).
-- f is called with the last element in the list and the accumulator,
-- that value is given as the accumulator to the next to last value and so on.
-- If we take f to be + and the starting accumulator value to be 0, that's 3 + (4 + (5 + (6 + 0))).
-- Or if we write + as a prefix function, that's (+) 3 ((+) 4 ((+) 5 ((+) 6 0))).
-- Similarly, doing a left fold over that list with g as the binary function and z as the accumulator
-- is the equivalent of g (g (g (g z 3) 4) 5) 6.

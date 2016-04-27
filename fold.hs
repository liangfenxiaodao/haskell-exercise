-- Fold

-- We'd introduce the x:xs pattern and then we'd do some action that involves a single element and the rest of the list.
-- It turns out this is a very common pattern, so a couple of very useful functions were introduced to encapsulate it.
-- These functions are called folds.

-- A fold takes a binary function, a starting value (I like to call it the accumulator) and a list to fold up.
-- The binary function itself takes two parameters.
-- The binary function is called with the accumulator and the first (or last) element and produces a new accumulator.
-- Then, the binary function is called again with the new accumulator and the now new first (or last) element, and so on.
-- Once we've walked over the whole list, only the accumulator remains, which is what we've reduced the list to.

-- with Fold, this implementation can be simplified to ]

ele ::(Eq a) => a -> [a] -> Bool
ele a [] = False
ele a (x:xs)
  | a == x = True
  | otherwise = a `ele` xs

ele' ::(Eq a) => a -> [a] -> Bool
ele' y ys = foldl (\acc x -> if x == y then True else acc) False ys

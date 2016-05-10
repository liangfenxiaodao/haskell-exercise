module Recursion where
  
fb ::(Eq a, Num a) => a -> Integer
fb x
  | x == 0 = 0
  | x == 1 = 1
  | otherwise = fb (x -1) + fb (x -2)

fbarray::(Eq a, Num a) => a -> [Integer]
fbarray x
  | x == 0 = [0]
  | x == 1 = [1, 0]
  | otherwise = (fb x):fbarray(x-1)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse'(xs) ++ [x]

zip' ::[a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs)(y:ys) = [(x, y)] ++ zip'(xs)(ys)

ele ::(Eq a) => a -> [a] -> Bool
ele a [] = False
ele a (x:xs)
  | a == x = True
  | otherwise = a `ele` xs

quicksort::(Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smaller = quicksort [a | a <- xs, a <= x]
      bigger  = quicksort [a | a <- xs, a > x]
  in smaller ++ [x] ++ bigger

-- Usually you define an edge case and then you define a function that does something between some element
-- and the function applied to the rest.
-- It doesn't matter if it's a list, a tree or any other data structure.
-- A sum is the first element of a list plus the sum of the rest of the list.
-- A product of a list is the first element of the list times the product of the rest of the list.
-- The length of a list is one plus the length of the tail of the list. Ekcetera, ekcetera ...


-- Generally, if you have a function like foo a = bar b a, you can rewrite it as foo = bar b, because of currying.

-- If we take into account that functions are curried, we can write the following implementation

-- sum' :: (Num a) => [a] -> a
-- sum' xs = foldl (\acc x -> acc + x) 0 xs

-- ever more succinctly, like

-- sum' :: (Num a) => [a] -> a
-- sum' = foldl (\acc x -> acc + x) 0

-- and then
-- sum' = foldl (+) 0

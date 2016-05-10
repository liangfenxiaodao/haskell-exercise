module Optional where
  
findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs

-- The function above is pretty simple, but when the key cannot be find in the list, then it will apply the `head` function on an empty list, which throws a runtime error.

-- Then we need to use optional here, with "Maybe", "Just", "Nothing" keywords

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key [] = Nothing
findKey' key ((k,v):xs) = if key == k 
                          then Just v
                          else findKey' key xs
                          
-- And we can use "fold" to simplify recursion

findKey2 :: (Eq k) => k -> [(k, v)] -> Maybe v  
findKey2 key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing 

-- Still, we cannot use foldl in the code above, because a "Maybe v" doesn't match the required type (k, v) in the foldl function.


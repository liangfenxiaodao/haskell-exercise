module Chapter5 where
  
-- Type Class and Concrete type

-- Prelude> let fifteen = 15
-- Prelude> :t fifteen
-- fifteen :: Num a => a
-- Prelude> let fifteenInt = fifteen :: Int
-- Prelude> let fifteenDouble = fifteen :: Double
-- Prelude> :t fifteenInt
-- fifteenInt :: Int
-- Prelude> :t fifteenDouble
-- fifteenDouble :: Double
funcIgnoresArgs :: a -> a -> a -> String
funcIgnoresArgs x y z = "Blah"

-- -- Remember
-- a -> a -> a -> String
-- -- is actually
-- a -> (a -> (a -> String))
-- -- So applying each argument
-- -- produces the following progression a -> a -> a -> String
-- a -> a -> String
-- a -> String
-- String

-- let h::(Num a, Num b) => a -> b -> b; h=undefined
-- :t h 1.0 2
-- h 1.0 2 :: Num b => b
-- because the variable b can have any type provided it satisfies the properties being a number

-- let g::(Num a, Num b) => a -> b -> b; g=undefined
-- :t g 1 (5.5 :: Double)
-- g 1 (5.5 :: Double)::Double
-- now we know the return type is Double


-- Type inference
-- let f x y=x+y+3
-- :t f
-- let f x y = x ++ y
-- :t f

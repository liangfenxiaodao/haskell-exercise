module DataType where

data Mood = Blah | Woot deriving Show

test :: Mood -> String
test = (\x -> "Hello")

greetIfCool :: String -> IO()
greetIfCool coolness =
  if cool coolness
    then putStrLn "I'm sooooo jealous"
  else
    putStrLn "Well.."
  where cool input = input == "walking around San Francisco for the whole day"

-- Prelude> :t (/)
-- (/) :: Fractional a => a -> a -> a
-- The notation Fractional a => denotes a typeclass constraint.
-- You can read it as “the type variable 𝑎 must implement the Fractional typeclass.”
-- This type information is telling us that whatever type of number 𝑎 turns out to be, i
-- t must be a type that has an instance of the Fractional typeclass.

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

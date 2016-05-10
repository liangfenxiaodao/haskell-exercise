module Chapter3 where

myGreeting :: String
myGreeting = "Hello" ++ " world"

hello :: String
hello = "Hello"

world :: String
world = " world"

main :: IO()
main = do
      putStrLn myGreeting
      putStrLn secondGreeting
      where secondGreeting = concat[ hello, world]

area d = pi * (r * r) where r = d / 2


-- Given "Curry is awesome!"
-- Return "y"
result = (drop 4 (take 5 "Curry is awesome!"))

-- Write a function of type String -> Char which returns the third character in a String.
theThirdString :: String -> Char
theThirdString x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

-- Using the take and drop functions we looked at above, see if you can write a function called rvrs
rvrs :: String
input = "Curry is awesome"
rvrs = x ++ " " ++ y ++ " " ++ z
    where z = (take 5 input)
          y = (take 2 (drop 6 input))
          x = (drop 9 input)

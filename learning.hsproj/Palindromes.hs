module Palindromes where
  respondPalindromes = unlines. map (\xs -> if isPalindromes xs then "Palindromes" else "Not Palindromes") . lines
    where isPalindromes xs = xs == reverse xs
  main = interact respondPalindromes
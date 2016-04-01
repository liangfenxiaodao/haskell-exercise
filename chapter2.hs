module Learn where

a = (\x y -> x * 3 + y)(3)(1000)

b         = x * 3 + y
  where x = 3
        y = 1000

-- let x = 1; y = 1000 in x * 3 + y

-- c = (\x y -> x * 5)(10 * 5 + y)(10)

d         = x * 5
  where x = 10 * 5 + y
        y = 10

-- let x = 7; y = negate x; z = y * 10 in z / x + y

e = z / x + y
  where x = 7
        y = negate x
        z = y * 10

-- f = (\x y z -> z / x + y)(7)(negate x)(y * 10)

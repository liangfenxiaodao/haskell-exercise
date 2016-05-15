module TypeClass 
(Point(..)
 ,Shape1(..)
 ,surface
 ,nudge
) where
  
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 x2 y1 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- Value constructors are functions, so we can map them and partially apply them and everything. If we want a list of concentric circles with different radii, we can do this.

-- map (Circle 10 20) [4,5,6,6]  

data Point = Point Float Float deriving (Show)
data Shape1 = Circle1 Point Float | Rectangle1 Point Point deriving (Show)

surface1 :: Shape1 -> Float
surface1 (Circle1 _ r) = pi * r ^ 2
surface1 (Rectangle1 (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape1 -> Float -> Float -> Shape1
nudge (Circle1 (Point x y) r) a b = Circle1 (Point (x+a) (y+b)) r
nudge (Rectangle1 (Point x1 y1) (Point x2 y2)) a b = Rectangle1 (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- Remember, value constructors are just functions that take the fields as parameters and return a value of some type (like Shape) as a result. 

-- So we can opt out not to export any value constructors for Shape by just writing Shape in the export statement: module TypeClass (Point(..), Shape1 ,surface ,nudge) where

-- Data.Map uses that approach. You can't create a map by doing Map.Map [(1,2),(3,4)] because it doesn't export that value constructor. However, you can make a mapping by using one of the auxilliary functions like Map.fromList. 

-- Not exporting the value constructors of a data types makes them more abstract in such a way that we hide their implementation. 

data Person = Person {
  firstName:: String, 
  lastName:: String, 
  age:: Int, 
  height:: Float, 
  phoneNumber:: String, 
  flavor:: String
} deriving (Show)



-- data Maybe a = Nothing | Just a 
-- Maybe is a type constructor and a is a type parameter
-- Maybe represents an option of either having nothing or having one of something. It doesn't matter what the type of that something is

-- Another example of a parameterized type that we've already met is Map k v from Data.Map. The k is the type of the keys in a map and the v is the type of the values. This is a good example of where type parameters are very useful. 



data Vector a = Vector a a a deriving (Show)

-- When declaring a data type, the part before the = is the type constructor and the constructors after it (possibly separated by |'s) are value constructors

vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector a b c) (Vector e f g) = Vector (a+e) (b+f) (c+g)

-- don't put type constraints into data declarations even if it seems to make sense, because you'll have to put them into the function type declarations either way. Just like vplus

data Man = Man {name :: String} deriving (Eq)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
            deriving (Show, Eq, Ord, Bounded, Enum)


-- class is for defining new typeclasses and instance is for making our types instances of typeclasses. the concept of instance is almost the same as extension


-- when you run
-- Prelude> :i Maybe
-- then you will see
-- data Maybe a = Nothing | Just a  -- Defined in ‘GHC.Base’
-- instance Eq a => Eq (Maybe a) -- Defined in ‘GHC.Base’
-- instance Functor Maybe -- Defined in ‘GHC.Base’
-- that's because Eq wants a concrete type while Functor want a type constuctor that takes one type, and Maybe is a type constructor.

-- We know Functor want a type constructor because the following definition tells us, "f" is used as the type of a value in a function: 
-- class Functor f where   
--    fmap :: (a -> b) -> f a -> f b  




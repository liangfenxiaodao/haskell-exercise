module Exercise where

-- Prelude> :t print
-- print :: Show a => a -> IO ()
-- As we see, print takes an argument 𝑎
-- that is an instance of the Show typeclass and returns an IO () result.
-- This result is an IO action that returns a value of the type ().
--
-- Typeclass deriving Typeclass instances we can magically derive are Eq, Ord, Enum, Bounded, Read, and Show,
-- though there are some con- straints on deriving some of these.
-- Deriving means you don’t have to manually write instances of these typeclasses for each new datatype you create.
-- We’ll address this a bit more in the chapter on Algebraic Datatypes.

-- Write the Eq instance for the datatype provided.
data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
    (==) (TisAn value)
         (TisAn value') =
         value == value'

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
    (==) (Two value1 value2)
         (Two value1' value2') =
         value1 == value1' && value2 == value2'

data StringOrInt = TisAnInt Int | TisAString String
data Pair a = Pair a a
data Tuple a b = Tuple a b

data Which a = ThisOne a | ThatOne a
data EitherOr a b = Hello a | Goodbye b


-- function can have multiple typeclass constraint, like
-- addWeird :: (Ord a, Num a) => a -> a -> a
-- addWeird x y = if x > 1 then x + y else x
-- if we define the function like addWeird :: Num a => a -> a -> a
-- then we will get error : Could not deduce (Ord a) arising from a use of ‘>’ from the context (Num a)
-- because '>' is a function that belongs to typeclass Ord

-- :info Int
-- data Int = GHC.Types.I# GHC.Prim.Int# 	-- Defined in ‘GHC.Types’
-- instance Bounded Int -- Defined in ‘GHC.Enum’
-- instance Enum Int -- Defined in ‘GHC.Enum’
-- instance Eq Int -- Defined in ‘GHC.Classes’
-- instance Integral Int -- Defined in ‘GHC.Real’
-- instance Num Int -- Defined in ‘GHC.Num’
-- instance Ord Int -- Defined in ‘GHC.Classes’
-- instance Read Int -- Defined in ‘GHC.Read’
-- instance Real Int -- Defined in ‘GHC.Real’
-- instance Show Int -- Defined in ‘GHC.Show’

-- and because Int type has the typeclasses Num, Eq, and Ord all implemented
-- then addWeird :: Int -> Int -> Int will pass type check

newtype Nada = Nada Double deriving (Eq, Show)
instance Num Nada where
  (+) (Nada x) (Nada y) = Nada (x + y)
  (*) (Nada x) (Nada y) = Nada (x * y)
  abs (Nada x) = Nada(abs(x))
instance Fractional Nada where
  (Nada x) / (Nada y) = Nada (x / y)
  recip (Nada n) = Nada (recip n)
  fromRational r = Nada (fromRational r)

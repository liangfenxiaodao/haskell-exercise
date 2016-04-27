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

-- Data constructors in Haskell provide a means of creating values that inhabit a given type.
-- Data constructors in Haskell have a type and can either be constant values (nullary) or take one or more arguments just like functions.
-- In the following example, Cat is a nullary data constructor for Pet and Dog is a data constructor that takes an argument:
--     -- Why name a cat? They don't answer anyway.
--     type Name = String
--     data Pet = Cat | Dog Name
-- The data constructors have the following types:
--     Prelude> :t Cat
--     Cat :: Pet
--     Prelude> :t Dog
--     Dog :: Name -> Pet
--
-- Type constructors in Haskell are not values and can only be used in type signatures.
-- Just as data declarations generate data constructors to create values that inhabit that type,
-- data declarations generate type constructors which can be used to denote that type.
-- In the above example, Pet is the type constructor.
-- A guideline for differentiating the two kinds of constructors is that type constructors always go to the left  of the = in a data declaration.
--
-- Data declarations define new datatypes in Haskell.
-- Data declarations always create a new type constructor,
-- but may or may not create new data constructors.
-- Data declarations are how we refer to the entire definition that begins with the data keyword.
--
--
-- A type alias is a way to refer to a type constructor or type constant by an alternate name,
-- usually to communicate something more specific or for brevity.
-- type Name = String
--     -- creates a new type alias Name of the
--     -- type String *not* a data declaration,
--     -- just a type alias declaration

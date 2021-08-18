module Sym where

thing :: IO ()
thing = putStrLn "thing"

data Transposition = T (Int, Int)| Id

instance Show Transposition where
  show (T x) = show x
  show Id = "Id"

instance Eq Transposition where
  Id == Id = True
  T (a,b) == T (c,d) = (a == c) && (b == d) || (a == d) && (b == c)
  _ == _ = False

newtype Element = E [Transposition]

instance Show Element where
  show (E []) = "[]"
  show (E list) = show list

-- Examples of elements using the format
elem1 :: Transposition
elem1 = Id

elem2 :: Transposition
elem2 = T (1,2)

elem3 :: Transposition
elem3 = T (2,3)

elem4 :: Transposition
elem4 = T (3,4)

x :: Element
x = E [elem1, elem2]

y :: Element
y = E [elem3, elem4, elem1]




-- Elements of the symmetric group will be represented by lists of transpositions OR by lists of disjoint cycles...
-- If there is a unique representation that can be obtained by a list of adjecent transpositions then we will use that

-- this defines the binary operation for the symmetric group by using transpositions
(<->) :: Element -> Element -> Element
(<->) (E [Id]) (E [Id]) = E [Id]
(<->) (E [Id]) (E [T (a,b)]) = E [T (a,b)]
(<->) (E [T (a,b)]) (E [Id]) = E [T (a,b)]
(<->) (E [T (a,b)]) (E [T (c,d)]) | T (a,b) == T (c,d) = E [Id]
                                  | a /= c && b /= d = E [T (a,b), T (c,d)]

-- (<->) (E perm1) (E perm2) | => we need an algorithm to simplify down elements to their unique normal form

-- This function takes an element and simplifies it down to its unique normal form
-- simplify :: Element -> Element
-- simplify (E list) =

data Cycle = C [Int]

instance Show Cycle where
  show (C list) = show list

cycle1 :: Cycle
cycle1 = C [1,2,3]

cycle2 :: Cycle
cycle2 = C [4,5]

cycle3 :: Cycle
cycle3 = C [2,4]

data Normalized = N [Cycle]

instance Show Normalized where
  show (N list) = show list

norm1 :: Normalized
norm1 = N [cycle1, cycle2, cycle3]




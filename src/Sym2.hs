import Data.List

data Cycle = C [Int] | Id

instance Show Cycle where
  show (C cycle) = show cycle
  show Id = "Id"

-- Perform operations on cycles

-- normalize cycles themselves (increasing order)
-- Normal form: smallest element in front and order stays same
-- step 1: find smallest element, step 2: reorder it so it stays consistent
smallest :: [Int] -> Int
smallest cycle = foldl (\acc x -> if x < acc then x else acc) (head cycle) cycle

normalizeList :: Int -> [Int] -> [Int]
normalizeList x list = [x] ++ afterx ++ beforex
                       where
                         beforex = takeWhile (/= x) list
                         afterx = reverse $ takeWhile (/= x) $ reverse list

normm :: [Int] -> [Int]
normm list = normalizeList (smallest list) list

normalize :: Cycle -> Cycle
normalize (C cycle) = C (normalizeList (smallest cycle) cycle)

isEquiv :: Cycle -> Cycle -> Bool
isEquiv (C cycle1) (C cycle2) = (normm cycle1) == (normm cycle2)

instance Eq Cycle where
  Id == Id = True
  C cycle1 == C cycle2 = isEquiv (C cycle1) (C cycle2)
  _ == _ = False

-- make cycles a member of Eq typeclass (by comparning their normal form)

c1 :: Cycle
c1 = C [1,2]

c2 :: Cycle
c2 = C [1,3]

c3 :: Cycle
c3 = C [2,3]

c4 :: Cycle
c4 = C [1,2,3]

c5 :: Cycle
c5 = C [1,3,2]

-- Define a datatype of Sym n
data Element = E [Cycle] | Identity

instance Show Element where
  show (E list) = show list

-- Define examples of elements which we can work with
e1 :: Element
e1 = E [c1]

e2 :: Element
e2 = E [c2]

e3 :: Element
e3 = E [c3]

e4 :: Element
e4 = E [c1,c3]

e5 :: Element
e5 = E [c4]

-- An element can be composed of one or many cycles BUT it needs to have a normal form
-- What does the normal form look like?
-- is [[1,2],[2,3]] a normal form or [[1,2,3]]? e4 == e5 is true
-- The normal form should take up the least amount of data so therefore we should be able to simplify these elements to Nothing or E [something]



instance Eq Element where
  Identity == E [Id] = True
  _ == _ = False

-- Normalize elements of Sym n (what's the normal form of an element in the symmetric group)














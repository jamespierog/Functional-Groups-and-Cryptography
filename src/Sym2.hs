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
c1 = C [1,2,3]

c1' :: Cycle
c1' = C [2,3,1]

c2 :: Cycle
c2 = C [5,3,4]

c3 :: Cycle
c3 = C [15,5,12,13,7]


-- Define a datatype of Sym n
data Element = E [Cycle] | Identity

instance Show Element where
  show (E list) = show list

e1 :: Element
e1 = E [c1, c2]

e2 :: Element
e2 = E [c3, c2]

-- Normalize elements of Sym n

-- Make them instances of Show and Eq typeclasses based on normal form







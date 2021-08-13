module Sym where

thing :: IO ()
thing = putStrLn "thing"

-- A function to define composition

data Transposition = T (Int, Int)

instance Show Transposition where
  show (T x) = show x


{-
instance Eq Transposition where
  T (a,b) == T (b,a) = True
  _       == _       = False
-}

data Element = Transposition | Id

sym2 :: [Transposition]
sym2 = [T (1,2), T (1,1)]

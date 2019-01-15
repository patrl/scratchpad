{-# LANGUAGE LambdaCase #-}

module InqSem where

import Data.List

-- powerset function
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x :) (powerset xs)


isNotEmptySet :: [a] -> Bool
isNotEmptySet [] = False
isNotEmptySet _ = True

powersetMinusEmpty :: [a] -> [[a]]
powersetMinusEmpty = filter isNotEmptySet . powerset

unions :: Eq a => [[a]] -> [a]
unions = nub . concat

data E
  = A
  | B
  deriving (Eq, Show, Enum)

dom :: [E]
dom = [A .. B]

type T = Bool

-- Worlds
data S = W1 | W2 | W3 | W4 deriving (Eq, Show, Bounded, Enum)

worlds :: [S]
worlds = [W1 .. W4]

leave :: S -> E -> T
leave = \case
  W1 -> \case
    A -> True
    B -> True
  W2 -> \case
    A -> True
    B -> False
  W3 -> \case
    A -> False
    B -> True
  W4 -> \case
    A -> False
    B -> False

newtype Inq a = Inq ([S] -> a)

inqLifter :: (S -> a) -> Inq a
inqLifter p = undefined

and :: Eq a => [a] -> [a] -> [a]
and = intersect

or :: Eq a => [a] -> [a] -> [a]
or = union

intNot :: [S] -> [S]
intNot = (\\) worlds

inqNot :: [[S]] -> [[S]]
inqNot q = powersetMinusEmpty (worlds \\ unions q)

-- >>> inqNot $ powersetMinusEmpty $ leave A
-- [[W4],[W3],[W3,W4]]
--
-- >>> intNot $ leave A
-- [W3,W4]
--
--
-- >>> powersetMinusEmpty $ leave B
-- [[W4],[W2],[W2,W4]]
--
--
-- >>> (powersetMinusEmpty $ leave A) `intersect` (powersetMinusEmpty $ leave B) 
-- [[W1]]
--
-- >>> (powersetMinusEmpty $ leave A) `union` (powersetMinusEmpty $ leave B)
-- [[W2],[W1],[W1,W2],[W3],[W1,W3]]
--
-- >>> :t nub
-- nub :: Eq a => [a] -> [a]

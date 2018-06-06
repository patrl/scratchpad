-- some helper functions to shift from function talk to set talk and vice versa, implemented using Haskell's singly-linked lists

module SetTalk where

import Control.Applicative (liftA2)

type T = Bool

-- Function for shifting from set to it's characteristic function
toCharFunc :: Eq a => [a] -> a -> T
toCharFunc xs x = elem x xs

-- >>> toCharFunc [1,2,3] $ 1
-- True

-- >>> toCharFunc [1,2,3] $ 4
-- False

-- Function for shifting from a set of pairs to a curried relation
toFunc2 :: (Eq a, Eq b) => [(a,b)] -> b -> a -> T
toFunc2 = flip . curry . toCharFunc

-- >>> (toFunc2 [(1,2)]) 2 1
-- True

-- >>> (toFunc2 [(1,2)]) 1 2
-- False

-- Function for shifting from a set of sets to a GQ.
toGQ :: Eq a => [[a]] -> (a -> T) -> T
toGQ q f = elem (toSet (concat q) f) q

-- powerset function
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

-- A function from a generalized quantifier q to a set of sets (given a domain dom).
-- note that this implementation only works with finite domains.
toSetOfSets :: Eq a => [a] -> ((a -> T) -> T) -> [[a]]
toSetOfSets dom q = [ xs | xs <- (powerset dom), q (toCharFunc xs) ]

someInt :: (Int -> T) -> T
someInt f = any f [1 .. 3]

noInt :: (Int -> T) -> T
noInt f = not $ any f [1 .. 3]

everyInt :: (Int -> T) -> T
everyInt f = all f [1 .. 3]

-- >>> toSetOfSets [1 .. 3] everyInt
-- [[1,2,3]]

-- >>> toSetOfSets [1 .. 3] noInt
-- [[]]

-- a function from a predicate and a domain, to the graph of the predicate
toGraph :: [a] -> (a -> T) -> [(a,T)]
toGraph dom f = [(x, (f x)) | x <- dom]

-- >>> take 5 $ toGraph [0 ..] even
-- [(0,True),(1,False),(2,True),(3,False),(4,True)]

-- a function from a predicate and a domain, to the set the predicate characterises relative to the domain.
toSet :: [a] -> (a -> T) -> [a]
toSet dom f = [x | (x,True) <-
                   toGraph dom f]

-- >>> take 3 $ toSet [0 ..] even
-- [0,2,4]

toGraph2 :: [a] -> [b] -> (b -> a -> T) -> [((b,a),T)]
toGraph2 domInt domExt f = [(x, (uncurry f $ x)) | x <- (liftA2 (,) domExt domInt)]

-- >>> toGraph2 [True,False] [True,False] (&&)
-- [((True,True),True),((True,False),False),((False,True),False),((False,False),False)]

-- >>> toGraph2 [True,False] [True, False] (||)
-- [((True,True),True),((True,False),True),((False,True),True),((False,False),False)]

a ==> b = not a || b

-- >>> toGraph2 [True,False] [True,False] (==>)
-- [((True,True),True),((True,False),False),((False,True),True),((False,False),True)]

toSet2 domInt domExt f = [x | (x,True) <- (toGraph2 domInt domExt f) ]

-- >>> toSet2 [True,False] [True,False] (&&)
-- [(True,True)]


-- >>> toSet2 [True,False] [True,False] (==>)
-- [(True,True),(False,True),(False,False)]

module CharacteristicFunctions where

import Model

  -- toGraph is the basic case, and generalizes to functions that don't end in T.

type Pred1 = Ent -> T

type Pred2 = Ent -> Ent -> T

toGraph :: Pred1 -> [(Ent,T)]
toGraph f = [(x,(f x)) | x <- dom]

toSet :: Pred1 -> [Ent]
toSet f = [x | x <- dom, f x == True]

toSet' :: Pred2 -> [(Ent,Ent)]
toSet' f = [(y,x) | x <- dom, y <- dom, (f x $ y) == True]

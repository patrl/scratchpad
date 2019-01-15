{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.State.Lazy
import Data.Functor.Identity
import Data.List (intersect)

type T = Bool

type U = StateT [S] Maybe

data Ent = Josie deriving (Eq, Show, Bounded, Enum)

-- Worlds
data S = W1 | W2 | W3 deriving (Eq, Show, Bounded, Enum)

-- assert :: U [S] -> U [S]
-- assert p = StateT (\c -> Just (q c, c' c `intersect` q c)) where
--   q c = fst $ runIdentity $ runStateT p c
--   c' c = snd $ runIdentity $ runStateT p c

_smoked :: Ent -> [S]

_smoked = \case
  Josie -> [W1,W2]

_smokes :: Ent -> [S]
_smokes = \case
  Josie -> [W1,W3]

_stopSmoking :: Ent -> [S] -> Maybe ([S],[S])
_stopSmoking x c = if c `subset` _smoked x then Just ([W2],c) else Nothing

subset :: Eq a => [a] -> [a] -> Bool
p `subset` q = null [x | x<-p , x `notElem` q]


module Juncts where

import Data.Function
import Data.Monoid

-- the model

type T = Bool

data Ent = Tom | Dick | Harry deriving (Eq, Show)

-- one place predicates

_left :: Ent -> T
_left Tom = True
_left _ = False

_isAngry :: Ent -> T
_isAngry Tom = False
_isAngry _ = True

newtype Conj = Conj T deriving (Eq, Show)

fromConj :: Conj -> T
fromConj (Conj t) = t

fromDisj :: Disj -> T
fromDisj (Disj t) = t

newtype Disj = Disj T deriving (Eq, Show)

instance Monoid Conj where
  (Conj True) `mappend` (Conj True) = Conj True
  _ `mappend` _ = Conj False
  mempty = Conj True

instance Monoid Disj where
  (Disj False) `mappend` (Disj False) = Disj False
  _ `mappend` _ = Disj True
  mempty = Disj False

-- There are no monoid instances for any other newtype wrappers around Bool!

j :: Monoid a => a -> a -> a
j = mappend

s1 = fromConj $ (Conj (Tom & _left)) & (j (Conj (Dick & _isAngry)))

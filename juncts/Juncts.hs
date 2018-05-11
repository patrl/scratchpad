module Juncts where

import Data.Function
import Data.Monoid
import Control.Applicative
import Control.Monad.Cont

-- the model

type T = Bool

data Ent = Tom | Dick | Harry deriving (Eq, Show)

-- one place predicates

-- _and :: (Cont T a) -> (Cont T a) -> (Cont T a)
-- _and m n = \k -> ((m k) && (n k))

_and = (&&)

_and' :: Cont T a -> Cont T a -> Cont T a
_and' f g = runCont (\q -> ((q (runCont f)) && (q (runCont g))))

returnBool = (return :: a -> Cont T a)

bindBool = ((>>=) :: Cont T a -> (a -> Cont T b) -> Cont T b)

-- >>> pure (&&)

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

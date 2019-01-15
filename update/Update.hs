{-# LANGUAGE OverloadedLists, LambdaCase #-}

module Update where

import           Control.Monad.State
-- import           Data.Functor.Identity
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

-- The type of truth values.
type T = Bool

-- The type of individuals.
data E = Hubert | Paul deriving (Eq, Show, Bounded, Enum)

-- The type of possible worlds.
data S = W1 | W2 | W3 | W4 deriving (Eq, Show, Bounded, Enum, Ord)

worlds :: Set S
worlds = [W1 .. W4]

-- The type of Stalnakerian updates.
-- type U = State (Set S)

type U = StateT (Set S) Maybe

-- One-place predicates

_smokesNow :: E -> Set S
_smokesNow = \case
  Paul   -> [W1, W2]
  Hubert -> [W1, W3]

_didSmoke :: E -> Set S
_didSmoke = \case
  Paul   -> [W1, W3]
  Hubert -> [W1, W2]

_vapes :: E -> Set S
_vapes = \case
  Paul   -> [W1, W2]
  Hubert -> [W3, W4]

_stoppedSmoking :: E -> U (Set S)
_stoppedSmoking x = StateT
  (\c -> if c `Set.isSubsetOf` _didSmoke x
    then Just (worlds Set.\\ _smokesNow x, c)
    else Nothing
  )

-- Takes a lifted Stalnakerian proposition, and turns it into a Stalnakerian assertion (i.e., a partial update of the common ground)
assert :: U (Set S) -> U (Set S)
assert m = StateT
  (\c ->
    let outputVal   = evalStateT m c
        outputState = execStateT m c
    in  case (outputVal, outputState) of
          (Just p, Just c') -> Just (p, c' `Set.intersection` p)
          (_     , _      ) -> Nothing
  )

statConj :: Set S -> Set S -> Set S
statConj = Set.intersection

-- a helper function to update the ignorance context
updIgnorance :: U (Set S) -> Maybe (Set S, Set S)
updIgnorance = ($ worlds) . runStateT

-- >>> (updIgnorance . assert . (return :: a -> U a)) (_vapes Paul)
-- Just (fromList [W1,W2],fromList [W1,W2])

-- >>> Set.fromList [W1 .. W4] Set.\\ _smokesNow Paul
-- fromList [W3,W4]

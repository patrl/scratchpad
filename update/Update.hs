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
_stoppedSmoking = toPresuppPred _didSmoke (propNeg <$> _smokesNow)


-- >>> propNeg <$> _smokesNow $ Paul
-- fromList [W3,W4]

toPresuppPred :: (E -> Set S) -> (E -> Set S) -> E -> U (Set S)
toPresuppPred presupp assertion x = StateT
  (\c -> if c `propEntails` presupp x then Just (assertion x, c) else Nothing)

-- Takes a lifted Stalnakerian proposition, and turns it into a Stalnakerian assertion (i.e., a partial update of the common ground)
assert :: U (Set S) -> U (Set S)
assert m = StateT
  (\c ->
    let outputVal   = evalStateT m c
        outputState = execStateT m c
    in  case (outputVal, outputState) of
          (Just p, Just c') -> Just (p, c' ∩ p)
          (_     , _      ) -> Nothing
  )

-- unicode set theory symbols for readability

(∩) :: Ord a => Set a -> Set a -> Set a
(∩) = Set.intersection

(∪) :: Ord a => Set a -> Set a -> Set a
(∪) = Set.union

(∖) :: Ord a => Set a -> Set a -> Set a
(∖) = (Set.\\)

(⊆) :: Ord a => Set a -> Set a -> Bool
(⊆) = Set.isSubsetOf

propNeg :: Set S -> Set S
propNeg = (∖) worlds

propEntails :: Set S -> Set S -> Bool
propEntails = (⊆)

propImplic :: Set S -> Set S -> Set S
p `propImplic` q = propNeg p ∪ q

-- a helper function to update the ignorance context
updIgnorance :: U (Set S) -> Maybe (Set S, Set S)
updIgnorance = ($ worlds) . runStateT

---
-- Heimian connectives.
--
-- N.b. that the following definitions assume that every propositional node is subject to
-- the "assert" operator.
---

heimNeg :: U (Set S) -> U (Set S)
heimNeg m = StateT
  (\c ->
    let outputVal   = evalStateT m c
        outputState = execStateT m c
    in  case (outputVal, outputState) of
          (Just p, Just c') -> Just (worlds ∖ p, c ∖ c')
          (_     , _      ) -> Nothing
  )

heimConj :: U (Set S) -> U (Set S) -> U (Set S)
m `heimConj` n = StateT
  (\c ->
    let interVal   = evalStateT m c
        interState = execStateT m c
    in  case (interVal, interState) of
          (Just p, Just c') ->
            let outVal   = evalStateT n c'
                outState = execStateT n c'
            in  case (outVal, outState) of
                  (Just q, Just c'') -> Just (p ∩ q, c'')
                  (_     , _       ) -> Nothing
          (_, _) -> Nothing
  )

heimDisj :: U (Set S) -> U (Set S) -> U (Set S)
m `heimDisj` n = StateT
  (\c ->
    let interVal   = evalStateT m c
        interState = execStateT m c
    in  case (interVal, interState) of
          (Just p, Just c') ->
            let outVal   = evalStateT n (c ∖ c')
                outState = execStateT n (c ∖ c')
            in  case (outVal, outState) of
                  (Just q, Just c'') -> Just (p ∪ q, c' ∪ c'')
                  (_     , _       ) -> Nothing
          (_, _) -> Nothing
  )

heimImplic :: U (Set S) -> U (Set S) -> U (Set S)
m `heimImplic` n = StateT
  (\c ->
    let interVal   = evalStateT m c
        interState = execStateT m c
    in  case (interVal, interState) of
          (Just p, Just c') ->
            let outVal   = evalStateT n c'
                outState = execStateT n c'
            in  case (outVal, outState) of
                  (Just q, Just c'') -> Just (worlds ∖ p ∪ q, c ∖ c' ∪ c'')
                  (_     , _       ) -> Nothing
          (_, _) -> Nothing
  )

-- "Paul did smoke and Paul stopped smoking." (presupposition satisfaction)

-- (a) with Heimian conjunction:
-- >>> updIgnorance $ (assert $ return $ _didSmoke Paul) `heimConj` (assert $ _stoppedSmoking Paul)
-- Just (fromList [W3],fromList [W3])

-- (b) with lifted static conjunction:
-- >>> updIgnorance $ (liftM2 (∩)) (assert $ return $ _didSmoke Paul) (assert $ _stoppedSmoking Paul)
-- Just (fromList [W3],fromList [W3])

-- "Paul stopped smoking and Paul did smoke." (presupposition failure)

-- (a) with Heimian conjunction:
-- >>> updIgnorance $ (assert $ _stoppedSmoking Paul) `heimConj` (assert $ return $ _didSmoke Paul)
-- Nothing

-- (b) with lifted static conjunction:
-- >>> updIgnorance $ (liftM2 (∩)) (assert $ _stoppedSmoking Paul) (assert $ return $ _didSmoke Paul)
-- Nothing

-- "Paul didn't stop smoking." (presupposition failure)

-- (a) with Heimian negation.
-- >>> updIgnorance $ heimNeg $ assert $ _stoppedSmoking Paul
-- Nothing

-- "Paul did smoke and Paul didn't stop smoking." (presupposition satisfaction)

-- (a) with Heimian negation.
-- >>> updIgnorance $ (assert $ return $ _didSmoke Paul) `heimConj` (heimNeg $ assert $ _stoppedSmoking Paul)
-- Just (fromList [W1],fromList [W1])

-- "If Paul did smoke then Paul stopped smoking." (presupposition satisfaction)
-- >>> updIgnorance $ (assert $ return $ _didSmoke Paul) `heimImplic` (assert $ _stoppedSmoking Paul)
-- Just (fromList [W2,W3,W4],fromList [W2,W3,W4])

-- "If Paul didn't smoke then Paul stopped smoking." (presupposition failure)
-- >>> updIgnorance $ (heimNeg $ assert $ return $ _didSmoke Paul) `heimImplic` (assert $ _stoppedSmoking Paul)
-- Nothing

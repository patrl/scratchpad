{-# LANGUAGE UnicodeSyntax #-}

module Intensionality where

import Model
import Charlow2018
import CharacteristicFunctions

  -- >>> toGraph1 ((fromG (_Λ Var_2 ((((pure _believe) <*> (_pro Var_2)) <*> (_Λ Var_1 (((pure _left) <*> (_pro Var_1)) <*> (pure Harry)))) <*> (pure Harry)))) _g6)
-- [(W1,False),(W2,True),(W3,False)]

 -- >>> toGraph1 $ (fromG (_Λ Var_1 (((pure _left) <*> (_pro Var_1)) <*> (pure Tom)))) _g6
-- [(W1,True),(W2,False),(W3,False)]

-- >>> toGraph1 (_left' Tom)
-- [(W1,True),(W2,False),(W3,False)]

-- Binding theory not very promising. How to derive locality?

-- Can we type-lift Each?

-- Need a function from ((a -> T) -> T) -> ((a -> (S -> T)) -> (S -> T))

each ∷ (Ent → T) → (Ent → T) → T
each p q = all (`elem` (toGraph1 q)) (toGraph1 p)

intensionalize ∷ ((Ent → T) → (Ent → T) → T) → ((Ent → (S → T)) → (Ent → (S → T)) → (S → T))
intensionalize d = \p → (\q → (\w → (d (flip p $ w) (flip q $ w))))

intensionalizeR ∷ ((Ent → T) → (Ent → T) → T) → ((Ent → T) → (Ent → (S → T)) → (S → T))
intensionalizeR d = \p → (\q → (\w → (d p (flip q $ w))))

-- >>> toGraph1 $ (intensionalize each) _isAngry' _left'
-- [(W1,*** Exception: Model.hs:(75,1)-(77,21): Non-exhaustive patterns in function _isAngry'

-- Now we can do de re with pronouns and otherwise everything automatically gets bound locally.

-- >>> toGraph1 $ ( _believe' $ (intensionalizeR each) (_boy' <*> (pure W1)) (_left')) Tom
-- [(W1,True),(W2,False),(W3,False)]

-- This makes interesting use of applicatives.

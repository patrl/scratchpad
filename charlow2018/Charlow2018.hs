{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables #-}

-- TODO: get binding reconstruction working
-- TODO: redefine application to make it compatible with composed applicatives.


module Charlow2018 where

import Model
import Data.Functor.Compose

type Pro a b = Var → G a b

type Pro' b = Var → G' b

-- Simple type-flexible pronoun
_pro ∷ Pro a a
_pro n = G (\g → g n)

-- doubly assignment-sensitive pro
_pro' ∷ Pro' Ent
_pro' n = Compose $ G (\h → (G (\g → g n)))

 -- _pro' :: Pro Ent (G Ent Ent)
 -- _pro' n = G (\(h :: Assignment Ent) -> (G (\(g :: Assignment Ent) -> g n))

-- G is the generalized type constructor for assignment sensitive meanings.
newtype G a b = G ((Assignment a) → b)

-- The Functor instance for the type constructor G a
instance Functor (G a) where
  fmap f (G b) = G (\n -> (f (b n)))

-- The applicative instance for the type constructor G a
instance Applicative (G a) where
  pure b = G (\g -> b)
  (<*>) (G aToB) (G a) = G (\g -> ((aToB g) (a g)))

type G' = Compose (G Ent) (G Ent)

-- Using ρ to ape Charlow's notation for pure.
ρ ∷ b → G a b
ρ = pure

-- Using (⍟) to ape Charlow's notation for <*>
(⍟) ∷ (G c (a → b)) → (G c a) → (G c b)
(⍟) = (<*>)

-- A helper function for taking an assignment function g, and returning a modified assignment function g' relative to a variable i and an individual x.
modify ∷ (Assignment a) → Var → a → (Assignment a)
modify g i x = g' where
  g' j
    | j == i = x
    | otherwise = g j

-- Abstraction relative to a variable.
_Λ  ∷ Var → (G a b) → (G a (a → b))
_Λ n (G f) = G (\g → (\x → f (modify g n x)))

-- helper functions for the newtype wrapper
fromG ∷ (G a b) → (Assignment a) → b
fromG (G f) = f

-- helper function for the Compose newtype wrapper
fromCompose :: Compose f g a → f (g a)
fromCompose (Compose a) = a

-- Subject raising (p. 3, Fig. 2)
-- >>> ((fromG (((_Λ Var_1) $ (ρ _left) ⍟ (_pro Var_1)) ⍟ (ρ Tom))) _g3)
-- True

-- Quantificational binding
-- >>> (fromG ((ρ _eachOfTomAndHarry) ⍟ (_Λ Var_1 (((ρ _likes) ⍟ ((ρ _brother) ⍟ (_pro Var_1))) ⍟ (_pro Var_1))))) _g4
-- True
-- (((pure . pure) _likes) <*> ((_pro' :: Pro Ent (G Ent Ent)) Var_1))
-- (((pure . pure) _likes) <*> ((_pro' :: Pro Ent (G Ent Ent)) Var_1))
--   :: G Ent (Ent -> Ent -> T)

  -- >>> :t (pure (pure _likes)) <*> ((_pro' :: Pro Ent (G Ent Ent)) Var_1)
-- (pure (pure _likes)) <*> ((_pro' :: Pro Ent (G Ent Ent)) Var_1)
--   :: G Ent (Ent -> Ent -> T)

-- >>> :t ((pure . pure) ((pure _brother) <*> (_pro Var_2)))
-- ((pure . pure) ((pure _brother) <*> (_pro Var_2)))
--   :: (Applicative f2, Applicative f1) => f1 (f2 (G Ent Ent))

  -- >>> :i ((_pro' :: Pro Ent (G Ent Ent)) Var_1)
-- <interactive>:1:2: error: parse error on input ‘(’

 -- _pro' :: Pro Ent (G Ent Ent)

-- Binding reconstruction

-- >>> :t (((pure :: b -> G' b) _eachOfTomAndHarry) <*> (Compose ((_Λ Var_1 (fromCompose (((pure :: b -> G' b) _likes) <*> (_pro' Var_2)))))))

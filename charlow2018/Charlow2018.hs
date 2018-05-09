{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables #-}

-- TODO: get binding reconstruction working

module Charlow2018 where

import Model
import Data.Functor.Compose
import Control.Applicative

-- Contexts

_g1 ∷ Assignment Ent
_g1 Var_1 = Tom
_g1 Var_2 = Dick
_g1 Var_3 = Harry

_g2 ∷ Assignment Ent
_g2 Var_1 = Tom
_g2 Var_2 = Tom
_g2 Var_3 = Tom

_g3 ∷ Assignment Ent
_g3 Var_1 = Tom
_g3 Var_2 = Dick
_g3 Var_3 = Dick

_g4 ∷ Assignment Ent
_g4 Var_1 = Tom
_g4 Var_2 = Harry
_g4 Var_3 = Dick

_g5 ∷ Assignment (G Ent Ent)
_g5 Var_1 = G (\g -> _brother (g Var_2))

type Pro a b = Var → G a b

-- Simple type-flexible pronoun
-- pronouns have a unitary semantics
_pro ∷ Pro a a
_pro n = G (\g → g n)

_proPaycheck = (_pro ∷ Pro (G Ent Ent) (G Ent Ent))

_proDP n = (_pro ∷ Pro Ent Ent) n

_proVP n = (_pro ∷ Pro (Ent → T) (Ent → T)) n

-- G is the generalized type constructor for assignment sensitive meanings.
newtype G a b = G ((Assignment a) → b)

-- The Functor instance for the type constructor G a
instance Functor (G a) where
  fmap f (G b) = G (\n -> (f (b n)))

-- The applicative instance for the type constructor G a
instance Applicative (G a) where
  pure b = G (\g -> b)
  (<*>) (G aToB) (G a) = G (\g -> ((aToB g) (a g)))

-- Composition of G (G Ent Ent) and G Ent. Introduces an additional newtype wrapper.
type G' = Compose (G (G Ent Ent)) (G Ent)

-- Using ρ to ape Charlow's notation for pure.
ρ ∷ b → G a b
ρ = pure

pure' ∷ b → G' b
pure' = pure

-- Using (⍟) to ape Charlow's notation for <*>
(⍟) ∷ (G c (a → b)) → (G c a) → (G c b)
(⍟) = (<*>)

tie' ∷ (G' (a → b)) → (G' a) → (G' b)
tie' = (<*>)

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
fromCompose ∷ Compose f g a → f (g a)
fromCompose (Compose a) = a

-- Subject raising (p. 3, Fig. 2)
-- >>> ((fromG (((_Λ Var_1) $ (ρ _left) ⍟ (_pro Var_1)) ⍟ (ρ Tom))) _g3)
-- True

-- Quantificational binding
-- >>> (fromG ((ρ _eachOfTomAndHarry) ⍟ (_Λ Var_1 (((ρ _likes) ⍟ ((ρ _brother) ⍟ (_pro Var_1))) ⍟ (_pro Var_1))))) _g4
-- True

-- Paycheck pronoun
-- >>> :t ((fromG ((fromG (fromCompose ((tie') (Compose ((pure (_Λ Var_2)) <*> (fromCompose ((tie') ((tie' (pure' _likes) (Compose (_proPaycheck Var_1)))) (Compose (pure (_pro Var_2))))))) (pure' Dick)))) _g5)) _g3)
-- ((fromG ((fromG (fromCompose ((tie') (Compose ((pure (_Λ Var_2)) <*> (fromCompose ((tie') ((tie' (pure' _likes) (Compose (_proPaycheck Var_1)))) (Compose (pure (_pro Var_2))))))) (pure' Dick)))) _g5)) _g3)
--   :: T

-- Variable free derivation applicative style. Takes advantage of pre-existing applicative instance for ((->) Ent).
-- >>> :t (pure _left) <*> (id :: Ent -> Ent)
-- (pure _left) <*> (id :: Ent -> Ent) :: Ent -> T

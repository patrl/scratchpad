{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables #-}

-- TODO Binding reconstruction etc.
-- TODO figure out how to generate possible contexts automatically -- djinn might be useful, or maybe co-arbitrary in quickcheck.

module Charlow2018 where

-- The model:

-- Variables
data Var = Var_1 | Var_2 | Var_3 deriving (Eq, Show)

data Ent = Tom | Dick | Harry deriving (Eq, Show)

-- Truth values
type T = Bool

-- (type flexible) Assignments
type Assignment a = Var → a

type G = Assignment Ent

-- One-place predicates

_left ∷ Ent → T
_left Tom = True
_left _ = False

_isAngry ∷ Ent → T
_isAngry Tom = False
_isAngry _ = True

-- Two-place predicates

_likes ∷ Ent → Ent → T
_likes Harry Tom = True
_likes Tom Harry = True
_likes Tom Tom = True
_likes Harry Harry = True
_likes _ _ = False

-- The brother function

_brother ∷ Ent → Ent
_brother Tom = Harry
_brother Harry = Tom

-- Quantifiers

_eachOfTomAndHarry ∷ (Ent → T) → T
_eachOfTomAndHarry f = (f Tom == True) && (f Harry == True)

-- Contexts

g1 ∷ Assignment Ent
g1 Var_1 = Tom
g1 Var_2 = Dick
g1 Var_3 = Harry

g2 ∷ Assignment Ent
g2 Var_1 = Tom
g2 Var_2 = Tom
g2 Var_3 = Tom

g3 ∷ Assignment Ent
g3 Var_1 = Tom
g3 Var_2 = Dick
g3 Var_3 = Dick

g4 ∷ Assignment Ent
g4 Var_1 = Tom
g4 Var_2 = Harry
g4 Var_3 = Dick

g5 ∷ Assignment ((Assignment Ent) → Ent)
g5 Var_1 = \g -> _brother (g Var_2)

pro ∷ Var → ((Assignment a) → a)
pro n = \g → g n

 -- Higher-order pronoun meaning
pro' ∷ Var → Assignment a → Assignment a → a
pro' n = \g → (\h → h n)

  -- We can just use the applicative instance declaration for ((→) a).
ρ ∷ a → ((Assignment c) → a)
ρ = pure

(⍟) ∷ ((Assignment c) → (a → b)) → ((Assignment c) → a) → ((Assignment c) → b)
(⍟) = (<*>)

-- A helper function for taking an assignment function g, and returning a modified assignment function g' relative to a variable i and an individual x.
modify ∷ (Assignment a) → Var → a → (Assignment a)
modify g i x = g' where
  g' j
    | j == i = x
    | otherwise = g j

-- categorematic abstraction (p. 5, definition 13), relative to a variable n.
abstraction ∷ Var → ((Assignment b) → a) → (Assignment b) → b → a
abstraction n f = \g → (\x → f (modify g n x))

μ:: ((Assignment a) → (Assignment a) → a) → (Assignment a) → a
μ m = \g -> (m g) g

-- Subject raising (p. 3, Fig. 2)
-- >>> ((abstraction Var_1) $ (ρ _left) ⍟ (pro Var_1)) ⍟ (ρ Tom) $ g3

-- >>> ((ρ _eachOfTomAndHarry) ⍟ (abstraction Var_1 (((ρ _likes) ⍟ ((ρ _brother) ⍟ (pro Var_1))) ⍟ (pro Var_1)))) g4
-- True

  -- Binding reconstruction:
  -- "His brother, each of Tom and Harry likes"
  -- >>> ((abstraction Var_2 ((ρ _eachOfTomAndHarry) ⍟ (abstraction Var_3 (((ρ _likes) ⍟ (μ (pro' Var_2))) ⍟ (pro Var_3))))) ⍟ ((ρ _brother) ⍟ (pro Var_3))) g1
-- True

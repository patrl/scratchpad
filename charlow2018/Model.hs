{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables #-}

module Model where

-- The model:

-- Variables
data Var = Var_1 | Var_2 | Var_3 deriving (Eq, Show)

data Ent = Tom | Dick | Harry deriving (Eq, Show)

-- Truth values
type T = Bool

-- (type flexible) Assignments
type Assignment a = Var → a

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


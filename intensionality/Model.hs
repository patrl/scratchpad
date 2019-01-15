{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables #-}

module Model where

import CharacteristicFunctions

-- The model:

-- Worlds
data S = W1 | W2 | W3 deriving (Eq, Show, Bounded, Enum)

-- Variables
data Var = Var_1 | Var_2 | Var_3 deriving (Eq, Show)

data Ent = Tom | Dick | Harry deriving (Eq, Show, Bounded, Enum)

-- Truth values
type T = Bool

-- (type flexible) Assignments
type Assignment a = Var → a

-- attitude verbs

_believe ∷ S → (S → T) → Ent → T
_believe W1 f Tom
  | (charFunc1 f) == [W1 ..] = True
  | otherwise = False

_believe W2 f Tom
  | (charFunc1 f) == [W2] = True
  | otherwise = False

_believe W3 f Tom
  | (charFunc1 f) == [W1,W3] = True
  | otherwise = False

_believe _ f Dick
  | (charFunc1 f) == [W1 ..] = True
  | otherwise = False

_believe W1 f Harry
  | (charFunc1 f) == [W1,W3] = True
  | otherwise = False

_believe W2 f Harry
  | (charFunc1 f) == [W1 ..] = True
  | otherwise = False

_believe W3 f Harry
  | (charFunc1 f) == [W2] = True
  | otherwise = False

_believe' ∷ (S → T) → Ent → (S → T)
_believe' f Tom w
  | (charFunc1 f) == [W1 ..] && w == W1 = True
  | otherwise = False


-- One-place predicates

_boy' ∷ Ent → S → T

_boy' _ _ = True

_left' ∷ Ent → S → T

_left' Tom w
  | w == W1 = True
  | otherwise = False

_left ∷ S → Ent → T

_left W1 Tom = True
_left W1 _ = False

_left W2 Tom = False
_left W2 _ = True

_left W3 _ = False

_isAngry' ∷ Ent → S → T

_isAngry' Tom w
  | w == W1 = True
  | otherwise = False

_isAngry ∷ S → Ent → T

_isAngry W1 Tom = False
_isAngry W1 _ = True

_isAngry W2 Tom = True
_isAngry W2 _ = False

_isAngry W3 _ = False


-- Two-place predicates

_likes ∷ S → Ent → Ent → T

_likes W1 Harry Tom = True
_likes W1 Tom Harry = True
_likes W1 Tom Tom = True
_likes W1 Harry Harry = True
_likes W1 _ _ = False

_likes W2 _ _ = True

_likes W3 _ _ = False

-- The brother function

_brother ∷ S → Ent → Ent

_brother W1 Tom = Harry
_brother W1 Harry = Tom

_brother W2 Dick = Tom
_brother W2 Tom = Dick

-- Quantifiers

_eachOfTomAndHarry ∷ (Ent → T) → T
_eachOfTomAndHarry f = (f Tom == True) && (f Harry == True)


-- Local Variables:
-- dante-command-line: ("nix-shell" "--run" "cabal repl --builddir=dist/dante")
-- End:

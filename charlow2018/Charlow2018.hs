{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables #-}

module Charlow2018 where

-- TODO Binding reconstruction etc.

-- The model:

-- Variables
data Var = Var_1 | Var_2 | Var_3 deriving (Eq, Show)

-- The domain
data Ent = Tom | Dick | Harry deriving (Eq, Show)

-- Truth values
type T = Bool

-- (type flexible) Assignments
type Assignment a = Var -> a

-- One-place predicates

_left :: Ent -> T
_left Tom = True
_left _ = False

_isAngry :: Ent -> T
_isAngry Tom = False
_isAngry _ = True

-- Two-place predicates

_likes :: Ent -> Ent -> T
_likes Harry Tom = True
_likes Tom Harry = True
_likes Tom Tom = True
_likes Harry Harry = True
_likes _ _ = False

-- The brother function

_brother :: Ent -> Ent
_brother Tom = Harry
_brother Harry = Tom

-- Quantifiers

_everyone :: (Ent -> T) -> T
_everyone f = (f Tom == True) && (f Dick == True) && (f Harry == True)

-- Contexts

g1 :: Assignment Ent
g1 Var_1 = Tom
g1 Var_2 = Dick
g1 Var_3 = Harry

g2 :: Assignment Ent
g2 Var_1 = Tom
g2 Var_2 = Tom
g2 Var_3 = Tom

g3 :: Assignment Ent
g3 Var_1 = Tom
g3 Var_2 = Dick
g3 Var_3 = Dick

g4 :: Assignment Ent
g4 Var_1 = Tom
g4 Var_2 = Harry
g4 Var_3 = Dick

pro :: Var -> (Assignment a) -> a
pro n = \g -> g n

  -- We can just use the applicative instance declaration for ((->) a).
ρ :: a -> ((Assignment c) -> a)
ρ = pure

(⍟) :: ((Assignment c) -> (a -> b)) -> ((Assignment c) -> a) -> ((Assignment c) -> b)
(⍟) = (<*>)

-- TODO: the flattener (p. 8, definition 19)
-- μ :: (Assignment (Assignment a)) -> (Assignment a)
-- μ m = \g -> ((m g) g)

-- A helper function for taking an assignment function g, and returning a modified assignment function g' relative to a variable i and an individual x.
modify :: (Assignment a) -> Var -> a -> (Assignment a)
modify g i x = g' where
  g' j
    | j == i = x
    | otherwise = g j

-- categorematic abstraction (p. 5, definition 13), relative to a variable n.
abstraction :: Var -> ((Assignment b) -> a) -> (Assignment b) -> b -> a
abstraction n f = \g -> (\x -> f (modify g n x))

-- Subject raising (p. 3, Fig. 2)
-- >>> ((abstraction Var_1) $ (ρ _left) ⍟ (pro Var_1)) <*> (ρ Tom) $ g3

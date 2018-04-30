module Charlow2018 where

-- TODO allow for assignments of different types

data Var = Var_1 | Var_2 | Var_3 deriving (Eq, Show)

data Ent = Tom | Dick | Harry deriving (Eq, Show)

type T = Bool

type Assignment = Var -> Ent

_left :: Ent -> T
_left Tom = True
_left _ = False

_isAngry :: Ent -> T
_isAngry Tom = False
_isAngry _ = True

_likes :: Ent -> Ent -> T
_likes Harry Tom = True
_likes Tom Harry = True
_likes Tom Tom = True
_likes Harry Harry = True
_likes _ _ = False

g1 :: Assignment
g1 Var_1 = Tom
g1 Var_2 = Dick
gt Var_3 = Harry

g2 :: Assignment
g2 Var_1 = Tom
g2 Var_2 = Tom
g2 Var_3 = Tom

g3 :: Assignment
g3 Var_1 = Tom
g3 Var_2 = Dick
g3 Var_3 = Dick

g4 :: Assignment
g4 Var_1 = Tom
g4 Var_2 = Harry
g4 Var_3 = Dick

pro :: Var -> (Assignment -> Ent)
pro n = \g -> g n

-- type Pro = (Assignment -> Ent) | (Assignment -> Pro)

  -- We can just use the applicative instance declaration for ((->) a).
ρ :: a -> (Assignment -> a)
ρ = pure

(⍟) :: (Assignment -> (a -> b)) -> (Assignment -> a) -> (Assignment -> b)
(⍟) = (<*>)

modify :: Assignment -> Var -> Ent -> Assignment
modify g i x = g' where
  g' j
    | j == i = x
    | otherwise = g j

abstraction :: Var -> (Assignment -> a) -> Assignment -> Ent -> a
abstraction n f = \g -> (\x -> f (modify g n x))

-- Subject raising
-- >>> ((abstraction Var_1) $ (ρ _left) ⍟ (pro Var_1)) <*> (ρ Tom) $ g3
-- True

-- >>> :t (((abstraction Var_1) $ (ρ _left) ⍟ (pro Var_1)) <*> (ρ Tom))
-- (((abstraction Var_1) $ (ρ _left) ⍟ (pro Var_1)) <*> (ρ Tom))
--   :: Assignment -> T

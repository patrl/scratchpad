{-# LANGUAGE LambdaCase #-}

module Chierchia where

import Data.Coerce
import Data.Functor.Compose
import Control.Monad (replicateM)
import Control.Applicative (liftA2)
import Control.Lens
import Data.Monoid (All)

-- Montague's intensional logic

-- Model theory

data E = A | B | C deriving (Eq, Show, Enum)

dom :: [E]
dom = [A .. C]

type T = Bool

instance Monoid Bool where
  mempty = True
  p `mappend` q = p && q

data S = W0 | W1 | W2 deriving (Eq, Show, Enum)

-- Variables are just integers
type Var = Int

-- Contexts (i.e. assignment functions) are treated as sequences and parameterised to a type.
type C a = [a]

-- Type constructor for assignment-sensitivity.
newtype G b a = G ((C b) -> a)

fromG :: G b a -> ((C b) -> a)
fromG = coerce

-- (Static) pronouns are functions from an index n, and a sequence as, to the nth element of as.
pro :: Var -> G a a
pro n = G (\as -> (as !! n))

-- Functor, applicative, and monad instances for context-sensitive meanings.

instance Functor (G a) where
  fmap f (G x) = G $ fmap f x

instance Applicative (G a) where
  pure x = G $ \c -> x
  (G f) <*> (G x) = G $ f <*> x

instance Monad (G a) where
  return = pure
  (G x) >>= f = G (\c -> ((coerce (f (x c))) c))

-- Type constructor for discourse-marker sensitive meanings
newtype Dm a = Dm ((C E) -> a)

instance Functor Dm where
  fmap f (Dm x) = Dm $ fmap f x

instance Applicative Dm where
  pure x = Dm $ \c -> x
  (Dm f) <*> (Dm x) = Dm $ f <*> x

fromDm :: Dm a -> ((C E) -> a)
fromDm = coerce

-- >>> charFunc1 $ ((pure leaveExt) <*> (proDm 0)) `andDm` ((pure leaveExt) <*> (proDm 1))
-- [[A,A,A],[A,A,B],[A,A,C]]

andDm :: Dm T -> Dm T -> Dm T
andDm = liftA2 (&&)

-- helper functions

toSetOfSets :: ((E -> T) -> T) -> [[E]]
toSetOfSets q = [ xs | xs <- (powerset dom), q (toCharFunc xs) ]

toCharFunc :: Eq a => [a] -> a -> T
toCharFunc xs = (\x -> elem x xs)

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)


-- Discourse markers are functions from an index n, and a sequences of entities as, to the nth element of as.
proDm :: Var -> Dm E
proDm n = Dm (\as -> (as !! n))

-- Intensionality:

newtype Intension a = Intension (S -> a)

fromInt :: Intension a -> (S -> a)
fromInt = coerce

instance Functor Intension where
  fmap f (Intension a) = Intension $ fmap f a

instance Applicative Intension where
  pure a = Intension $ \w -> a
  (Intension f) <*> (Intension x) = Intension $ f <*> x

instance Monad Intension where
  return = pure
  (Intension a) >>= f = (Intension (\w -> ((coerce (f (a w))) w)))

assignments = [ xs | xs <- (replicateM (length dom) dom) ]

leaveInt :: E -> Intension T
leaveInt = \case
  A -> (Intension (\case
           W0 -> True
           W1 -> False
           W2 -> True ))
  B -> (Intension (\case
           W0 -> True
           W1 -> False
           W2 -> False ))
  C -> (Intension (\case
           W0 -> True
           W1 -> True
           W2 -> False ))

boyExt :: E -> T
boyExt = \case
  A -> True
  B -> False
  C -> False

girlExt :: E-> T
girlExt = \case
  A -> False
  _ -> True

leaveExt :: E -> T
leaveExt = \case
  A -> True
  B -> False
  C -> False

leaveDyn :: E -> Dm T -> Dm T
leaveDyn x p = (liftA2 (&&) (pure (leaveExt x)) p)

aExt :: (E -> T) -> (E -> T) -> T
aExt p q = (not . null) [x | x <- dom, p x && q x]

-- aDyn :: (E -> Dm T -> Dm T) -> (E -> Dm T -> Dm T) -> Dm T -> Dm T
-- aDyn p q = \p -> 


allAssignments :: C E -> T
allAssignments = toCharFunc assignments

onlyBAssignments = toCharFunc [[B,B,B]]

newtype CCC a = CCC (Dm T -> Dm a)

instance Functor CCC where
  fmap f (CCC x) = CCC $ (fmap . fmap) f x

instance Applicative CCC where
  pure x = (CCC (\p -> ((Dm (\c -> x)))))
  (<*>) = undefined


-- Showing how Chierchia's system builds on an ordinarily context-sensitive system.
-- >>> charFunc1 $ Dm ((\p -> (\c -> (((fromDm ((pure leaveExt) <*> (proDm 0))) c) && (p c)))) allAssignments)
-- [[A,A,A],[A,A,B],[A,A,C],[A,B,A],[A,B,B],[A,B,C],[A,C,A],[A,C,B],[A,C,C]]



-- >>> aExt girlExt leaveExt
-- False

worlds :: [S]
worlds = [W0 .. W2]

-- >>> toList ((pure leaveInt) <*> (pro 0))
-- [([A,A,A],W0),([A,A,A],W2),([A,A,B],W0),([A,A,B],W2),([A,A,C],W0),([A,A,C],W2),([A,B,A],W0),([A,B,A],W2),([A,B,B],W0),([A,B,B],W2),([A,B,C],W0),([A,B,C],W2),([A,C,A],W0),([A,C,A],W2),([A,C,B],W0),([A,C,B],W2),([A,C,C],W0),([A,C,C],W2),([B,A,A],W0),([B,A,B],W0),([B,A,C],W0),([B,B,A],W0),([B,B,B],W0),([B,B,C],W0),([B,C,A],W0),([B,C,B],W0),([B,C,C],W0),([C,A,A],W0),([C,A,A],W1),([C,A,B],W0),([C,A,B],W1),([C,A,C],W0),([C,A,C],W1),([C,B,A],W0),([C,B,A],W1),([C,B,B],W0),([C,B,B],W1),([C,B,C],W0),([C,B,C],W1),([C,C,A],W0),([C,C,A],W1),([C,C,B],W0),([C,C,B],W1),([C,C,C],W0),([C,C,C],W1)]


toGraph (G f) = [(x, (uncurry (fmap fromInt f) $ x)) | x <- (liftA2 (,) assignments worlds )]

toList p = [x | (x,True) <- (toGraph p) ]

toGraphDm (Dm f) = [(x, (f x)) | x <- assignments]

charFunc1 f = [x | (x,True) <- (toGraphDm f) ]

toGraphCC (Dm f) = [(x, (uncurry (fmap fromDm f) $ x)) | x <- (liftA2 (,) assignments assignments)]




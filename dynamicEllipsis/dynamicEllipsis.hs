module DynamicEllipsis where

import           Control.Lens                   ( element
                                                , set
                                                )
import           Control.Monad                  ( replicateM )
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State

data E
  = A
  | B
  | C
  deriving (Eq, Show, Enum)

data S
  = W1
  | W2
  | W3
  deriving (Eq, Show, Enum)

univ :: [S]
univ = [W1 .. W3]

dom :: [E]
dom = [A .. C]

type T = Bool

boy :: S -> E -> T
boy W1 A = True
boy W1 _ = False
boy W2 B = True
boy W2 _ = False
boy W3 C = True
boy W3 _ = False

girl :: E -> T
girl A = False
girl _ = True

leave :: S -> E -> T
leave W1 _ = True
leave W2 _ = False
leave W3 A = True
leave W3 _ = False

friend :: E -> E -> T
friend A B = True
friend B A = True
friend _ _ = False

-- -- The set of assignments.
assignments :: [[E]]
assignments = replicateM (length dom) dom

-- Variables are just integers
type Var = Int

-- Contexts (i.e. assignment functions) are treated as sequences and parameterised to a type.
type C a = [a]

-- Type constructor for assignment sensitive meanings, using ReaderT. G is a reader monad parameterised to assignments.
type G = ReaderT (C E)

-- Lexical entry for pronouns -- they take an index n and return the nth member of the assignment.
pro :: Var -> G Identity E
pro n = ReaderT (\as -> Identity (as !! n))

-- a function for lowering assignment sensitive meanings to ordinary montagovian meanings
lowerG :: C E -> G m a -> m a
lowerG g = ($ g) . runReaderT

-- helper functions for modified assignments
modifyG :: C E -> Var -> E -> C E
modifyG g n x = set (element n) x g

modified :: C E -> Var -> [C E]
modified g n = [ modifyG g n x | x <- dom ]

-- a function from a finite set to the characteristic function of this set.
toCharFunc :: Eq a => [a] -> a -> T
toCharFunc xs x = x `elem` xs

-- A function from a quantifier q with a finite domain to a set of sets.
toSetOfSets :: Eq a => [a] -> ((a -> T) -> T) -> [[a]]
toSetOfSets qDom q = [ xs | xs <- powerset qDom, q (toCharFunc xs) ]

powerset :: [a] -> [[a]]
powerset []       = [[]]
powerset (x : xs) = powerset xs ++ map (x :) (powerset xs)

-- a function from a predicate and a finite domain, to the graph of the predicate
toGraph :: [a] -> (a -> T) -> [(a, T)]
toGraph domvar f = [ (x, f x) | x <- domvar ]

-- a function from a predicate and a finite domain, to the set the predicate characterises relative to the domain.
toSet :: [a] -> (a -> T) -> [a]
toSet domvar f = [ x | (x, True) <- toGraph domvar f ]

-- pretty prints an assignment
prettyPrintAssignment :: C E -> IO ()
prettyPrintAssignment = putStrLn . unwords . map show

type StateSet = StateT (C E) []


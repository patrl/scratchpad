module DynCont where

import Control.Applicative (liftA2)
import Control.Lens (element, set)
import Control.Monad (replicateM)
import Control.Monad.Cont
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Coerce
import Data.Functor.Compose

data E
  = A
  | B
  | C
  deriving (Eq, Show, Enum)

dom :: [E]
dom = [A .. C]

type T = Bool

boy :: E -> T
boy A = True
boy _ = False

girl :: E -> T
girl A = False
girl _ = True

leave :: E -> T
leave C = False
leave _ = True

friend :: E -> E -> T
friend A B = True
friend B A = True
friend _ _ = False

-- -- The set of assignments.
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
modify :: C E -> Var -> E -> C E
modify g n x = set (element n) x g

modified g n = [modify g n x | x <- dom]

-- a function from a finite set to the characteristic function of this set.
toCharFunc :: Eq a => [a] -> a -> T
toCharFunc xs x = x `elem` xs

-- A function from a quantifier q with a finite domain to a set of sets.
toSetOfSets :: Eq a => [a] -> ((a -> T) -> T) -> [[a]]
toSetOfSets qDom q = [xs | xs <- powerset qDom, q (toCharFunc xs)]

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x :) (powerset xs)

-- a function from a predicate and a finite domain, to the graph of the predicate
toGraph :: [a] -> (a -> T) -> [(a, T)]
toGraph dom f = [(x, f x) | x <- dom]

-- a function from a predicate and a finite domain, to the set the predicate characterises relative to the domain.
toSet :: [a] -> (a -> T) -> [a]
toSet dom f = [x | (x, True) <- toGraph dom f]

-- pretty prints an assignment
prettyPrintAssignment :: C E -> IO ()
prettyPrintAssignment = putStrLn . unwords . map show

newtype CCP a = CCP { (>>-) :: (G Identity a -> G Identity T) -> G Identity T }

-- N.b. that CCP is the composition of reader and continuation monads. Functor and applicative instances follow from this definition.
-- type CCP' = Compose (Cont ([E] -> T)) ((->) [E])
instance Functor CCP where
  fmap f cx = CCP $ \k -> cx >>- \x -> k (fmap f x)

instance Applicative CCP where
  pure a = CCP $ \k -> k (pure a)
  cf <*> cx = CCP $ \k -> cf >>- \f -> cx >>- \x -> k (f <*> x)

-- Is dyanmicising one way of implementing this monad???
instance Monad CCP where
  return = pure
  cgx >>= f = CCP $ \k -> cgx >>- \gx -> gx >>= (\x -> f x >>- k)

-- categorematic abstraction
abstract :: Var -> G Identity a -> G Identity (E -> a)
abstract n (ReaderT f) = ReaderT (\g -> Identity $ runIdentity . f . modify g n)

-- Totally standard entry for existential quantification
ex :: Var -> G Identity T -> G Identity T
ex n (ReaderT p) =
  ReaderT
    (\g -> Identity (not . null $ filter (runIdentity . p) (modified g n)))
  -- Totally standard universal quantification.

forall :: Var -> G Identity T -> G Identity T
forall n (ReaderT p) =
  ReaderT (\g -> Identity (not $ any (not . runIdentity . p) (modified g n)))

-- >>> forall 0 $ (pure leave) <*> (pro 0)
-- <interactive>:7:2: error:
--     • No instance for (Show (G Identity T))
--         arising from a use of ‘print’
--     • In a stmt of an interactive GHCi command: print it
liftG :: G Identity a -> CCP a
liftG x = CCP $ \k -> k (ReaderT $ \g -> runReaderT x g)

-- liftG' gives a function from contexts to contexts ((like an existential quantifier) discourse-level scope
liftG' :: (G Identity T -> G Identity T) -> (CCP T -> CCP T)
liftG' f cp = CCP $ \k -> f (cp >>- k)

exDyn :: Var -> CCP T -> CCP T
exDyn n = liftG' (ex n)

aDyn :: Var -> CCP T -> CCP T -> CCP T
aDyn n dp cp = CCP $ \k -> ex n ((dp `conjDyn` cp) >>- k)

conjDyn :: CCP T -> CCP T -> CCP T
cp `conjDyn` cq = CCP $ \k -> cp >>- liftM2 (&&) (cq >>- k)

implicDyn :: CCP T -> CCP T -> CCP T
cp `implicDyn` cq = CCP $ \k -> cp >>- liftM2 (==>) (cq >>- k)

drefIntro :: Var -> E -> CCP T -> CCP T
drefIntro n x cp =
  CCP $ \k -> ReaderT $ \g -> runReaderT (cp >>- k) (modify g n x)

dynamicAbs :: Var -> CCP T -> E -> CCP T
dynamicAbs n cp x =
  CCP $ \k -> ReaderT $ \g -> runReaderT (cp >>- k) (modify g n x)

dynSomeone :: Var -> CCP E
dynSomeone n = CCP $ \k -> ex n . k $ pro n

dynIndef :: Var -> CCP (E -> T) -> CCP E
dynIndef n cp =
  CCP $ \k -> ex n $ (cp `ap` liftG (pro n)) >>- liftM2 (&&) (k $ pro n)

dynUniv :: Var -> CCP (E -> T) -> CCP E
dynUniv n cp =
  CCP $ \k -> forall n $ (cp `ap` liftG (pro n)) >>- liftM2 (==>) (k $ pro n)

-- dynUniv :: Var -> (CCP (E -> T)) -> CCP E
-- dynUniv n cp = CCP $ \k ->
-- material implication.
(==>) :: T -> T -> T
t ==> u = not t || u

-- >>> showAppContext $ (return leave) `ap` (dynIndef 0 (return boy))
-- [[A,A,A],[A,A,B],[A,A,C],[A,B,A],[A,B,B],[A,B,C],[A,C,A],[A,C,B],[A,C,C],[B,A,A],[B,A,B],[B,A,C],[B,B,A],[B,B,B],[B,B,C],[B,C,A],[B,C,B],[B,C,C],[C,A,A],[C,A,B],[C,A,C],[C,B,A],[C,B,B],[C,B,C],[C,C,A],[C,C,B],[C,C,C]]
-- >>>  showAppContext $ ((return leave) `ap` (dynSomeone 0)) `conjDyn` ((return boy) `ap` (liftG $ pro 0))
-- [[A,A,A],[A,A,B],[A,A,C],[A,B,A],[A,B,B],[A,B,C],[A,C,A],[A,C,B],[A,C,C],[B,A,A],[B,A,B],[B,A,C],[B,B,A],[B,B,B],[B,B,C],[B,C,A],[B,C,B],[B,C,C],[C,A,A],[C,A,B],[C,A,C],[C,B,A],[C,B,B],[C,B,C],[C,C,A],[C,C,B],[C,C,C]]
showAppContext ccp =
  toSet assignments $ fmap runIdentity $ runReaderT $ (>>-) ccp id-- >>> toSet assignments $ fmap runIdentity $ runReaderT $ (flip (>>-)) id $ ((pure leave) <*> (liftG (pro 0))) `conjDyn` (exDyn 0 $ (pure boy) <*> (liftG (pro 0)))
-- [[A,A,A],[A,A,B],[A,A,C],[A,B,A],[A,B,B],[A,B,C],[A,C,A],[A,C,B],[A,C,C],[B,A,A],[B,A,B],[B,A,C],[B,B,A],[B,B,B],[B,B,C],[B,C,A],[B,C,B],[B,C,C]]
-- >>> toSet assignments $ fmap runIdentity $ runReaderT $ (flip (>>-)) id $ (return leave) `ap` (liftG $ pro 0)
-- [[A,A,A],[A,A,B],[A,A,C],[A,B,A],[A,B,B],[A,B,C],[A,C,A],[A,C,B],[A,C,C],[B,A,A],[B,A,B],[B,A,C],[B,B,A],[B,B,B],[B,B,C],[B,C,A],[B,C,B],[B,C,C]]
-- >>> toSet assignments $ fmap runIdentity $ runReaderT $ (flip (>>-)) id $ (pure leave) <*> (liftG $ pro 0)
-- [[A,A,A],[A,A,B],[A,A,C],[A,B,A],[A,B,B],[A,B,C],[A,C,A],[A,C,B],[A,C,C],[B,A,A],[B,A,B],[B,A,C],[B,B,A],[B,B,B],[B,B,C],[B,C,A],[B,C,B],[B,C,C]]
-- >>> showAppContext $ (liftM2 friend) (dynIndef 1 ((return friend) `ap` (dynIndef 0 (return boy)))) (liftG $ pro 0)
-- [[A,A,A],[A,A,B],[A,A,C],[A,B,A],[A,B,B],[A,B,C],[A,C,A],[A,C,B],[A,C,C],[B,A,A],[B,A,B],[B,A,C],[B,B,A],[B,B,B],[B,B,C],[B,C,A],[B,C,B],[B,C,C],[C,A,A],[C,A,B],[C,A,C],[C,B,A],[C,B,B],[C,B,C],[C,C,A],[C,C,B],[C,C,C]]
-- >>> showAppContext $ (liftM2 friend) (dynUniv 1 ((return friend) `ap` (dynIndef 0 (return boy)))) (liftG $ pro 0)
-- [[A,A,A],[A,A,B],[A,A,C],[A,B,A],[A,B,B],[A,B,C],[A,C,A],[A,C,B],[A,C,C],[B,A,A],[B,A,B],[B,A,C],[B,B,A],[B,B,B],[B,B,C],[B,C,A],[B,C,B],[B,C,C],[C,A,A],[C,A,B],[C,A,C],[C,B,A],[C,B,B],[C,B,C],[C,C,A],[C,C,B],[C,C,C]]

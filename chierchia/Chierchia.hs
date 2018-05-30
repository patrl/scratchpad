-- from reader-based context-sensitivity to Chierchia's dynamic semantics via Monad Transformers
{-# LANGUAGE LambdaCase #-}

module Chierchia where

import Data.Coerce
import Control.Monad (replicateM)
import Control.Lens (set, element)
import Control.Applicative (liftA2)
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Identity

--
-- Basic extensional system
--

data E = A | B | C deriving (Eq, Show, Enum)

dom :: [E]
dom = [A .. C]

type T = Bool

boy :: E -> T
boy A = True
boy _ = False

girl :: E-> T
girl A = False
girl _ = True

leave :: E -> T
leave C = False
leave _ = True

--
-- Generalized conjunction via the continuation monad.
--

-- using the continuation monad, we characterize the programs terminating in T values.
type BoolT = ContT T Identity

-- generalized conjunction conjoins two programs that terminate in a boolean type.
andCT :: Cont T a -> Cont T a -> Cont T a
andCT cf cg = ContT $ \k ->
  Identity $ runCont cf $ \f ->
  runCont cg $ \g ->
  (runIdentity $ k f) && (runIdentity $ k g)

negCT :: Cont T a -> Cont T a
negCT cp = ContT $ \k ->
  Identity $ runCont cp $ \p ->
  not (runIdentity $ k p)

-- conjoins a program that terminates in a boolean type with a trivially continuationized truth proposition. This will come in handy later.
andHelper :: BoolT a -> BoolT T -> BoolT a
cf `andHelper` ct = ContT $ \k ->
  Identity $ runCont cf $ \f ->
  runCont ct $ \t ->
  (runIdentity $ k f) && t

-- lowers a trivially continuationized proposition
lower :: Cont T T -> T
lower cp = runCont cp id


--
-- Monadic context-sensitive system
--

-- -- The set of assignments.
assignments = [ xs | xs <- (replicateM (length dom) dom) ]

-- -- The characteristic function of the set of assignments.
assignmentsF :: G BoolT T
assignmentsF = ReaderT $ fmap return $ toCharFunc assignments

-- Variables are just integers
type Var = Int

-- Contexts (i.e. assignment functions) are treated as sequences and parameterised to a type.
type C a = [a]

-- Type constructor for assignment sensitive meanings, using the ReaderT constructor from mtl. G is a reader monad parameterised to assignments.
type G = ReaderT (C E)



-- Lexical entry for pronouns.
pro :: Var -> G Identity E
pro n = ReaderT (\as -> Identity (as !! n))

lowerG :: C E -> G m a -> m a
lowerG g = ($ g) . runReaderT

modify :: C E -> Var -> E -> C E
modify g n x = set (element n) x g

modified g n = [ modify g n x  | x <- dom ]

-- Standard existential quantification
ex :: Var -> G Identity T -> G Identity T
ex n (ReaderT p)
  = ReaderT (\g ->
               Identity (not . null $ filter
                         (\h -> runIdentity $ p h)
                         (modified g n)))

-- Continuationized existential quantification (will come in handy later)
exCont :: Var -> G BoolT T -> G BoolT T
exCont n (ReaderT p) =
  ReaderT $ \g -> ContT $ \k ->
  Identity (not . null $ filter (\h -> runCont (p h) $ \q ->
                                    runIdentity $ k q) (modified g n))

--
-- Chierchia's dynamic semantics using these ingredients.
--

-- This is Chierchia's type for a Context Change Potential - It takes a set of contexts, and returns a set of contexts.
newtype CCP a = CCP { runCCP :: ReaderT ((G BoolT T)) (G BoolT) a }

-- Lowers CCPs to continuations.
lowerCCP :: C E -> G BoolT T -> CCP a -> BoolT a
lowerCCP g p = lowerG g
               . ($ p)
               . runReaderT
               . runCCP

instance Functor CCP where

  fmap f x = CCP $ ReaderT $ \p ->
    ReaderT $ \g ->
    (fmap f (lowerCCP g p $ x)) `andHelper` (lowerG g $ p)

instance Applicative CCP where

  pure x = CCP $ ReaderT $ \(ReaderT p) ->
    ReaderT $ \g ->
    return x `andHelper` p g

  f <*> x = CCP $ ReaderT $ \p ->
    ReaderT $ \g ->
    ((lowerCCP g p $ f) <*> (lowerCCP g p $ x))
    `andHelper` (lowerG g $ p)

instance Monad CCP where

  return = pure

  x >>= f = CCP $ ReaderT $ \p ->
    ReaderT $ \g ->
    ((lowerCCP g p $ x) >>=
    \y ->
      lowerCCP g p $ f y) `andHelper` (lowerG g $ p)

-- function to lift from assignment-sensitive meanings to dynamic meanings.
liftG :: G Identity a -> CCP a
liftG (ReaderT x) = CCP $ ReaderT $ \(ReaderT p) ->
  ReaderT $ \g ->
  (return . runIdentity $ x g)
  `andHelper` (p g)


-- Chierchia's dynamic existential quantification is also treated as a kind of function composition
exCCP :: Var -> CCP T -> CCP T
exCCP n (CCP (ReaderT scope))
  = CCP $ ReaderT $ \p -> exCont n . scope $ p

-- Chierchia's dynamic conjunction is a treated as a kind of function composition.
andCCP :: CCP T -> CCP T -> CCP T
(CCP (ReaderT c1)) `andCCP` (CCP (ReaderT c2))
  = CCP $ ReaderT $ \p -> c1 . c2 $ p

-- negCCP
negCCP :: CCP a -> CCP a
negCCP prej = CCP $ ReaderT $ \p ->
  ReaderT $ \g ->
  negCT (lowerCCP g p $ prej)

-- Helper functions

toCharFunc :: Eq a => [a] -> a -> T
toCharFunc xs x = elem x xs

toSetOfSets :: Eq a => [a] -> ((a -> T) -> T) -> [[a]]
toSetOfSets qDom q = [ xs | xs <- (powerset qDom), q (toCharFunc xs) ]

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

toGraph :: [a] -> (a -> T) -> [(a,T)]
toGraph dom f = [(x, (f x)) | x <- dom]

toSet :: [a] -> (a -> T) -> [a]
toSet dom f = [x | (x,True) <-
                   toGraph dom f]

prettyPrintAssignment :: C E -> IO ()
prettyPrintAssignment = putStrLn
                        . unwords
                        . map show

showApGContext :: G BoolT T -> CCP T -> IO ()
showApGContext context = mapM_ prettyPrintAssignment
                 . toSet assignments
                 . fmap lower
                 . runReaderT
                 . ($ context)
                 . runReaderT
                 . runCCP


-- TODO: internal lift etc.

module CharlowDiss where

import Control.Monad.State
import Control.Monad.Cont
import Data.Function

-- Basic model theory

type T = Bool

data E = A | B | C deriving (Eq, Show, Enum)

dom :: [E]
dom = [A .. C]

boy :: E -> T
boy A = True
boy _ = False

girl :: E-> T
girl A = False
girl _ = True

leave :: E -> T
leave C = False
leave _ = True

hugs :: E -> E -> T
hugs A C = True
hugs C A = True
hugs A A = True
hugs A B = True
hugs _ _ = False

-- Monad for state-sensitivity and non-determinism

type StateSet = StateT [E] []

-- introduce a discourse referent
drefIntro :: StateSet E -> StateSet E
drefIntro m =
  m >>= \v ->
          StateT $ \i ->
                  [(v, (v : i))]

-- existential quantifier over the domain
ex :: StateSet E
ex = StateT $ \i ->
                [(x,i) | x <- dom ]

-- pronouns
pro :: StateSet E
pro = StateT $ \i ->
                   [(head i, i)]

-- dynamic negation
notDy :: StateSet T -> StateSet T
notDy (StateT s) = StateT $ \i ->
                            [((null [x | x <- s i, fst x]),i)]

-- monadic conjunction
andM :: Monad m => m T -> m T -> m T
andM = liftM2 (&&)

-- the scopal tier

(>>-) = runContT

-- continuationize a monadic program
liftM' :: Monad m => m a -> ContT r m a
liftM' m = ContT $ (>>=) m

-- universal quantifier
evDyCont :: ContT T (StateSet) E
evDyCont = ContT $ \k ->
                     notDy $ ex >>=
                     \x -> notDy $ k x

bind :: ContT r StateSet E -> ContT r StateSet E
bind m = ContT $ \k ->
                   m >>- \x ->
                           StateT $ \i ->
                                      (runStateT (k x)) (x : i)

-- feed a continuationized monadic program the trivial monadic continuation
lowerM :: (Monad m) => ContT a m a -> m a
lowerM = ($ return) . (>>-)

resetM :: (Monad m) => ContT a m a -> ContT r m a
resetM = liftM' . lowerM


-- forward application. Side-effects left-to-right.
(-/-) :: Monad m => m (a -> b) -> m a -> m b
(-/-) = ap

-- backwards application. Side-effects left-to-right
(-\-) :: Monad m => m a -> m (a -> b) -> m b
(-\-) = liftM2 (&)

-- forwards application. Side-effects right-to-left.
(-//-) :: Monad m => m (a -> b) -> m a -> m b
f -//- x = x -\- f

-- backwards application. Side-effects right-to-left.
(-\\-) :: Monad m => m a -> m (a -> b) -> m b
x -\\- f = f `ap` x


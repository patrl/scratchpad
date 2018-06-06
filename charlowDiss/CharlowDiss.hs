module CharlowDiss where

import Control.Monad.State
import Control.Monad.Cont
import Data.Function ((&))
-- TODO: use this module for tripartite towers
-- import qualified Control.Effect as Ix

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

liftM'' f m1 = do { x1 <- m1; liftM' (f x1) }

-- external lift is just liftM'
extLift :: ContT r m a -> ContT l (ContT r m) a
extLift = liftM'

-- internal lift -- add an intermediate continuation to a continuation
intLift m = ContT $ \c ->
  m >>- \v ->
  c (ContT $ \k ->
        k v)

lowerM' :: Monad m => ContT a m (ContT a m a) -> m a
lowerM' = lowerM . join

-- >>> ($ []) . runStateT . lowerM' $ ((return . bind . liftM' $ ex) --\-- ((return . return $ hugs) --/-- (intLift evDyCont)))
-- [(False,[])]

-- >>> ($ []) . runStateT . lowerM' $ (return evDyCont) --\-- ((return . return $ hugs) --/-- (intLift . bind . liftM' $ ex))
-- [(True,[A]),(False,[B]),(False,[C])]

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

-- higher-order forward application. Side-effects left-to-right
(--/--) :: (Monad m2, Monad m1) =>
  m1 (m2 (a -> b)) -> m1 (m2 a) -> m1 (m2 b)
(--/--) = liftM2 (-/-)

-- backwards application. Side-effects left-to-right
(-\-) :: Monad m => m a -> m (a -> b) -> m b
(-\-) = liftM2 (&)

-- higher-order backwards application. Side-effects left-to-right
(--\--) :: (Monad m2, Monad m1) =>
  m1 (m2 a) -> m1 (m2 (a -> b)) -> m1 (m2 b)
(--\--) = liftM2 (-\-)

  -- Can't we do inverse scope much more simply by changing the order in which side-effects are evaluated?

-- -- forwards application. Side-effects right-to-left.
-- (-//-) :: Monad m => m (a -> b) -> m a -> m b
-- f -//- x = x -\- f

-- -- backwards application. Side-effects right-to-left.
-- (-\\-) :: Monad m => m a -> m (a -> b) -> m b
-- x -\\- f = f `ap` x

-- >>> (liftM' notDy) -/- (resetM $ ((bind . liftM' $ ex) -\- ((return hugs) -/- evDyCont)))
-- <interactive>:150:19: error:
--     • Couldn't match type ‘StateSet T -> StateSet T’
--                      with ‘StateT [E] [] (T -> a)’
--       Expected type: StateSet (T -> a)
--         Actual type: StateSet T -> StateSet T
--     • Probable cause: ‘notDy’ is applied to too few arguments
--       In the first argument of ‘liftM'’, namely ‘notDy’
--       In the first argument of ‘(-/-)’, namely ‘(liftM' notDy)’
--       In the second argument of ‘($)’, namely
--         ‘(liftM' notDy)
--            -/-
--              (resetM $ ((bind . liftM' $ ex) -\- ((return hugs) -/- evDyCont)))’
--     • Relevant bindings include
--         it :: StateSet a (bound at <interactive>:150:2)

-- >>> return notDy

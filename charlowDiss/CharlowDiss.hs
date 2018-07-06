{-# LANGUAGE FlexibleInstances #-}

module CharlowDiss where

import Control.Monad.Indexed
import Control.Monad.State
import Data.Function ((&))
import Data.Functor.Indexed ((<<$>>),(<<*>>))

-- ----- --
-- model --
-- ----- --
  
type T = Bool

data E
  = A
  | B
  | C
  deriving (Eq, Show, Enum)

dom :: [E]
dom = [A .. C]

boy :: E -> T
boy A = True
boy _ = False

girl :: E -> T
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

-- ---------------- --
-- The dynamic tier --
-- -------------------

-- Monad for state-sensitivity and non-determinism
type StateSet = StateT [E] []

-- introduce a discourse referent
dref :: StateSet E -> StateSet E
dref m = m >>= \v -> StateT $ \i -> [(v, v : i)]

-- dynamic existential
ex :: StateSet E
ex = StateT $ \i -> [(x, i) | x <- dom]

-- pronoun
pro :: StateSet E
pro = StateT $ \i -> [(head i, i)]

-- dynamic negation
notDy :: StateSet T -> StateSet T
notDy (StateT s) = StateT $ \i -> [(null [x | x <- s i, fst x], i)]

-- ------------------
-- The scopal tier --
-- ------------------
-- We roll our own indexed continuation monad.
newtype IxKT n o m i = IxKT { (>>-) :: (i -> n m) -> n o }

instance IxFunctor (IxKT m) where
  imap f m = IxKT $ \k -> m >>- (k . f)

instance IxPointed (IxKT m) where
  ireturn a = IxKT ($a)

instance Monad m => IxApplicative (IxKT m) where
  iap = iapIxMonad

instance Monad m => IxMonad (IxKT m) where
  ibind f c = IxKT $ \k -> c >>- \a -> f a >>- k

instance Monad m => Functor (IxKT m o o) where
  fmap = imap

instance Monad m => Applicative (IxKT m o o) where
  pure = ireturn
  (<*>) = iap

instance Monad m => Monad (IxKT m o o) where
  return = ireturn
  m >>= k = ibind k m

-- forwards scopal application
rap :: Monad m => IxKT m o1 i1 (a -> b) -> IxKT m i1 i2 a -> IxKT m o1 i2 b
rap = (<<*>>)

-- backwards scopal application
lap :: Monad m => IxKT m o1 i1 a -> IxKT m i1 i2 (a -> b) -> IxKT m o1 i2 b
n `lap` m = IxKT $ \k -> n >>- \x -> m >>- \f -> k (x & f)

-- continuationize a monadic program
mLift :: Monad m => m a -> IxKT m o o a
mLift m = IxKT $ (>>=) m

-- Lower a continuationized monadic program
mLower :: (Monad m) => IxKT m b a a -> m b
mLower m = m >>- return

mReset :: (Monad m) => IxKT m b a a -> IxKT m o o b
mReset = mLift . mLower

bind :: IxKT StateSet o i E -> IxKT StateSet o i E
bind m = IxKT $ \k -> m >>- \x -> StateT $ \i -> runStateT (k x) (x : i)

evDyCont :: IxKT StateSet T T E
evDyCont = IxKT $ \k -> notDy $ ex >>= \x -> notDy $ k x

-- --------------
-- determiners --
-- --------------
-- r = restrictor
_a :: IxKT StateSet E T E
_a =
  IxKT $ \r ->
    StateT $ \i -> [(x, i') | x <- dom, (True, i') <- runStateT (r x) i]

-- r = restrictor; k = scope
_every :: (E -> StateSet T) -> IxKT StateSet T T E
_every r = IxKT $ \k -> notDy ((_a >>- r) >>= (notDy . k))

-- r = restrictor; k = scope
_no :: (E -> StateSet T) -> IxKT StateSet T T E
_no r = IxKT $ \k -> notDy ((_a >>- r) >>= k)

-- ---------------------
-- higher order scope --
-- ---------------------
-- higher-order forwards scopal application
rAp ::
     Monad m
  => IxKT m o2a i2a (IxKT m o1a i1a (a -> b))
  -> IxKT m i2a i2b (IxKT m i1a i1b a)
  -> IxKT m o2a i2b (IxKT m o1a i1b b)
m `rAp` n = IxKT $ \k -> m >>- \f -> n >>- \x -> k (f `rap` x)

-- higher-order backwards scopal application
lAp ::
     Monad m
  => IxKT m o2a i2a (IxKT m o1a i1a a)
  -> IxKT m i2a i2b (IxKT m i1a i1b (a -> b))
  -> IxKT m o2a i2b (IxKT m o1a i1b b)
n `lAp` m = IxKT $ \k -> n >>- \x -> m >>- \f -> k (x `lap` f)

-- external lift
extLift :: Monad m => IxKT m o1 i a -> IxKT m o2 o2 (IxKT m o1 i a)
extLift = ireturn

-- internal lift
intLift :: Monad m => IxKT m o1 i a -> IxKT m o1 i (IxKT m o2 o2 a)
intLift m = ireturn <<$>> m

-- lower a two-level tower in one fell swoop.
mLower2 :: Monad m => IxKT m a a (IxKT m a a a) -> m a
mLower2 = mLower . ijoin

-- -----------
-- examples --
-- -------- --

apEmptyStack = ($ []) . runStateT

_everyBoy = _every (return <$> boy)
_aGirl = mReset (_a `lap` (ireturn girl))
_aBoy = mReset (_a `lap` (ireturn boy))

-- ex 4.7, p. 94

-- >>> apEmptyStack $ mLower $ (mReset $ (bind _aGirl) `lap` ((ireturn hugs) `rap` _everyBoy)) `lap` ((ireturn (&&)) `rap` (mReset $ (mLift pro) `lap` (ireturn leave)))
-- [(True,[B]),(False,[C])]



-- >>> ($ []) . runStateT . mLower $ (mReset $ (mReset (_a `lap` (ireturn girl))) `lap` (ireturn leave))
-- [(True,[]),(False,[])]

-- >>> ($ []) . runStateT . mLower . mReset $ (_every (return <$> boy)) `lap` (ireturn leave)
-- [(True,[])]

-- surface scope

-- "A girl hugs every boy"

-- >>> ($ []) . runStateT . mLower $ (mReset (_a `lap` (ireturn girl))) `lap` ((ireturn hugs) `rap` (_every (return <$> boy)))
-- [(True,[]),(True,[])]

-- "Every boy hugs a girl"

-- >>> ($ []) . runStateT . mLower $ (_every (return <$> boy)) `lap` ((ireturn hugs) `rap` (mReset (_a `lap` (ireturn girl))))
-- [(True,[])]

-- inverse scope

-- "A girl hugs every boy"

--  >>> ($ []) $ runStateT (mLower2 ((extLift (mReset (_a `lap` (ireturn girl)))) `lAp` (intLift $ (ireturn hugs) `rap` (_every (return <$> boy)))))
-- [(True,[])]


-- "Every boy hugs a girl"

-- >>> ($ []) . runStateT . mLower2 $ (extLift (_every (return <$> boy))) `lAp` (intLift $ (ireturn hugs) `rap` (mReset (_a `lap` (ireturn girl))))
-- [(False,[]),(True,[])]

-- ------------------- --
-- extension: movement --
-- ----------------------

-- This works, but introduces additional monadic structure that we have to get rid of with `join`.
trace :: Monad m => IxKT m (E -> m a) a E
trace = IxKT $ return

-- This is what we want, but it's impossible to define.
trace2 :: Monad m => IxKT m (E -> a) a E
trace2 = IxKT $ undefined

-- "which girl does A hug?"

-- >>> apEmptyStack . join . mLower $ (bind _aGirl) `lap` (mReset ((ireturn A) `lap` ((ireturn hugs) `rap` trace)))
-- [(False,[B]),(True,[C])]

-- "Which boy hugs which girl"

-- >>> apEmptyStack $ mLower ((bind _aBoy) `lap` (mLift ((join $ mLower (join <<$>> ((bind _aGirl) `lap` (mReset (trace `lap` (mReset $ (ireturn hugs) `rap` trace)))))))))
-- [(False,[B,A]),(True,[C,A])]

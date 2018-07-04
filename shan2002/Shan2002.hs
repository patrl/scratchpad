{-# LANGUAGE FlexibleInstances #-}

module Shan2002 where

import Control.Monad.Indexed
import Control.Monad
import Data.Functor.Compose
import Control.Monad.State
import Data.Function ((&))

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

know :: T -> E -> T
know True _ = True
know _ _ = False

liftK :: K a b -> IxK a a b
liftK kx = IxK $ (>>-) kx

 -- Monad for state-sensitivity and non-determinism
type StateSet = StateT [E] []

-- introduce a discourse referent
drefIntro :: StateSet E -> StateSet E
drefIntro m = m >>= \v -> StateT $ \i -> [(v, v : i)]

-- existential quantifier over the domain
ex :: StateSet E
ex = StateT $ \i -> [(x, i) | x <- dom]

-- pronouns
pro :: StateSet E
pro = StateT $ \i -> [(head i, i)]


-- someone :: IxK T T E
someone = IxK $ \k -> any k dom

-- someone3 :: IxK (E -> t) T E
-- someone3 = IxK $ \k -> \x -> any (\x -> (k x)) dom

who = dom

-- Let's role out own continuation monad with handy infix notation
data K a b = K { (>>-) :: (b -> a) -> a }

lower :: K a a -> a 
lower kt = kt >>- id

instance Functor (K a) where
  fmap f kx =  K $ \k ->
                     kx >>- \x ->
                              k $ f x

instance Applicative (K a) where
  pure x =  K $ \k -> k x
  kf <*> kx = K $ \k ->
                    kf >>- \f ->
                             kx >>- \x ->
                                      k $ f x

instance Monad (K a) where
  return = pure
  kx >>= m = K $ \k ->
                   kx >>- \x ->
                            (m x) >>- \f ->
                                        k f

-- Now let's role our own indexed continuation monad
data IxK o m i = IxK { (>>>-) :: (i -> m) -> o }

instance IxFunctor IxK where
  imap f kx = IxK $ \k ->
                      kx >>>- (k . f)

instance IxPointed IxK where
  ireturn a = IxK $ ($a)

instance IxApplicative IxK where
  -- kf `iap` kx = IxK $ \k ->
  --                       kf >>>- \f ->
  --                                 kx >>>- \x ->
  --                                           k (f x)
  iap = iapIxMonad


instance IxMonad IxK where
  ibind f c = IxK $ \k ->
                      c >>>- \a ->
                               (f a) >>>- k

instance Functor (IxK o o) where
  fmap = imap

instance Applicative (IxK o o) where
  pure = ireturn
  (<*>) = iap

instance Monad (IxK o o) where
  return = ireturn 
  m >>= k = ibind k m

-- application for three-level indexed towers
m `iapp` n = IxK $ \k ->
                   m >>>- \f ->
                            n >>>- \x ->
                                     k (f `iap` x)


-- >>> :t iapp
-- iapp
--   :: IxApplicative m1 =>
--      IxK o m2 (m1 i j (a -> b))
--      -> IxK m2 m3 (m1 j k1 a) -> IxK o m3 (m1 i k1 b)


data IxKT n o m i = IxKT { (>>>>-) :: (i -> n m) -> n o }

instance IxFunctor (IxKT m) where
  imap f m = IxKT $ \k ->
                       m >>>>- (k . f)

instance IxPointed (IxKT m) where
  ireturn a = IxKT ($a)


instance Monad m => IxApplicative (IxKT m) where
  iap = iapIxMonad

instance Monad m => IxMonad (IxKT m) where
  ibind f c = IxKT $ \k ->
                       c >>>>- \a ->
                                 (f a) >>>>- k

instance Monad m => Monad (IxKT m o o) where
  return = ireturn
  m >>= k = ibind k m

liftSet :: [a] -> IxKT [] o o a
liftSet ma = IxKT $ (>>=) ma

liftStateSet :: StateSet a -> IxKT StateSet o o a
liftStateSet ma = IxKT $ (>>=) ma


instance Monad m => Functor (IxKT m o o) where
  fmap = imap

instance Monad m => Applicative (IxKT m o o) where
  pure = ireturn
  (<*>) = iap
  
-- Double movement

-- >>> (((ireturn ((((ireturn hugs) `iap` trace) `iap` trace) >>>- id)) `iap` someone) `iap` someone) >>>- id
-- True

-- need to do a HUGE amount of lifting.
-- >>> (>>>- id) $ (>>>- id) <$> ((liftM2 iap) (ireturn <$> ((>>>- id) <$> ((liftM2 iap) (ireturn <$> ((ireturn hugs) `iap` someone)) (ireturn trace)))) (ireturn someone))
-- True

-- >>> :t (((liftM2 iap) (ireturn <$> ((ireturn hugs) `iap` someone)) (ireturn trace)))
-- (((liftM2 iap) (ireturn <$> ((ireturn hugs) `iap` someone)) (ireturn trace)))
--   :: IxK Bool Bool (IxK (E -> k1) k1 T)

-- how do I close this off????

trace2 :: IxK ((IxK Bool Bool E) -> a) a (IxK Bool Bool E)
trace2 = IxK $ id
-- trace2 = IxK $ id

trace :: IxK (E -> a) a E
trace = IxK $ id

-- >>> :t ireturn <$> trace
-- ireturn <$> trace
--   :: (IxPointed m2, Functor (IxK (a -> m1) m1)) =>
--      IxK (a -> m1) m1 (m2 i i a)

-- >>> :t (>>>- id) $ (ireturn ((>>>- id) (imap (>>>- id) ((ireturn ((ireturn hugs) `iap` someone)) `iapp` (imap ireturn trace))))) `iap` someone
-- (>>>- id) $ (ireturn ((>>>- id) (imap (>>>- id) ((ireturn ((ireturn hugs) `iap` someone)) `iapp` (imap ireturn trace))))) `iap` someone
--   :: Bool

-- 

-- multiple question
-- >>> (return ((>>>- id) ((`ap` who) `imap` (return `imap` ((>>>- id) `imap` ((ireturn ((ireturn hugs) `iap` trace)) `iapp` (ireturn `imap` trace))))))) `ap` who
-- [[True,False,True],[True,False,False],[True,False,False]]

-- Baker ambiguity

which :: IxKT [] E T E
which = IxKT $ \f -> [x | x <- dom, True `elem` f x]


-- >>> (((ireturn hugs) `iap` (liftSet (((ireturn boy) `iap` which) >>>>- return))) `iap` which) >>>>- return
-- [A,B,C]

-- >>> (return (((ireturn leave) `iap` trace) >>>- id)) `ap` ((((ireturn hugs) `iap` (liftSet (((ireturn boy) `iap` which) >>>>- return))) `iap` which) >>>>- return)
-- [True,True,False]


-- >>> ($ [A]) $ runStateT $ (>>>>- return) $ (liftStateSet ex) >>= ((((ireturn . ireturn $ hugs) `iapp` (ireturn `imap` trace)) `iapp` (ireturn . liftStateSet $ pro)) >>>- id)
-- [(True,[A]),(False,[A]),(True,[A])]


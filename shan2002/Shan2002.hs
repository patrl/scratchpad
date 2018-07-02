{-# LANGUAGE FlexibleInstances #-}

module Shan2002 where

import Control.Monad.Indexed
import Control.Monad

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

liftK :: K a b -> IxK a a b
liftK kx = IxK $ (>>-) kx

someone :: IxK T T E
someone = IxK $ \k -> any k dom

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
  kf `iap` kx = IxK $ \k ->
                        kf >>>- \f ->
                                  kx >>>- \x ->
                                            k (f x)



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

instance Monad m => Functor (IxKT m o o) where
  fmap = imap

instance Monad m => Applicative (IxKT m o o) where
  pure = ireturn
  (<*>) = iap
 
trace :: IxK (E -> a) a E
trace = IxK $ undefined

traceT :: Monad m => IxKT m (E -> m a) a E
traceT = IxKT $ return

lowerSet :: [a] -> Maybe a
lowerSet [x] = Just x
lowerSet _ = Nothing

who :: [E]
who = dom

-- >>> :t ((((ireturn hugs) `iap` traceT) `iap` (ireturn A)) >>>>- return) `ap` who
-- ((((ireturn hugs) `iap` traceT) `iap` (ireturn A)) >>>>- return) `ap` who
--   :: [[T]]


  





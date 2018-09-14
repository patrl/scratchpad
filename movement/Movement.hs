module Movement where

import Data.Functor.Compose

data E
  = A
  | B
  | C
  deriving (Eq, Show, Enum)

dom :: [E]
dom = [A .. C]

type T = Bool

boy :: Arg E T
boy A = True
boy _ = False

girl :: Arg E T
girl A = False
girl _ = True

leave :: Arg E T
leave C = False
leave _ = True

friend :: Arg E (Arg E T)
friend _ A = True
friend _ _ = False
  
someone :: K T E
someone = K $ \k -> any k dom

who :: [E]
who = dom

everyone :: K T E
everyone = K $ \k -> all k dom

-- Let's role out own continuation monad with handy infix notation
newtype K a b = K
  { (>>-) :: (b -> a) -> a }

lower :: K a a -> a
lower kt = kt >>- id

instance Functor (K a) where
  fmap f kx = K $ \k -> kx >>- \x -> k $ f x

instance Applicative (K a) where
  pure x = K $ \k -> k x
  kf <*> kx = K $ \k -> kf >>- \f -> kx >>- \x -> k $ f x

instance Monad (K a) where
  return = pure
  kx >>= m = K $ \k -> kx >>- \x -> m x >>- \f -> k f

type Arg a = (->) a

type M a b = Compose (Arg a) (K b)

type Move m a = Compose (Arg a) m

trace :: Arg E a -> Move (K b) E a
trace f = Compose $ \x -> K $ \k -> k (f x)

whTrace :: Arg E a -> Move [] E a
whTrace f = Compose $ \x -> [f x]

traceGeneral :: Applicative m => Arg b a -> Move m b a
traceGeneral f = Compose $ \x -> pure (f x)
-- Movement derivation:
 -- >>> lower $ someone >>= (getCompose $ (trace friend) <*> (pure A))
-- True
-- Non-movement derivation:
-- >>> lower $ ((pure friend) <*> someone) <*> (pure A)
-- True
-- Two things move
-- deriving scope rigidity with movement.
-- >>> lower $ lower <$> ((>>=) everyone) <$> (getCompose <$> (someone >>= (getCompose $ (pure trace) <*> (trace friend))))
-- False
-- >>> lower $ someone >>= (getCompose $ (liftA2 (&&)) ((trace friend) <*> (pure A)) ((trace friend) <*> (pure B)))
-- False
-- >>> who >>= (getCompose $ (whTrace friend) <*> (pure A))
-- [True,True,True]
-- wh-movement and in-situ
-- >>>  lower $ lower <$> ((liftM2) (>>=)) (return someone) (getCompose <$> (liftM trace) ((pure friend) <*> someone))
-- True

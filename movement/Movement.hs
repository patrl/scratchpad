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

boy :: Arg T
boy A = True
boy _ = False

girl :: Arg T
girl A = False
girl _ = True

leave :: Arg T
leave C = False
leave _ = True

friend :: Arg (Arg T)
friend A B = True
friend B A = True
friend _ _ = False

someone :: K T E
someone = K $ \k -> any k dom

everyone :: K T E
everyone = K $ \k -> all k dom

-- Let's role out own continuation monad with handy infix notation
data K a b = K { (>>-) :: (b -> a) -> a }

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
                   kx >>- \x -> undefined
                            

type Arg = (->) E

type M = Compose Arg (K T)

trace :: Arg a -> M a
trace f = Compose $ \x ->
                      K $ \k ->
                            k (f x)

test = getCompose $ (trace friend) <*> (pure A)

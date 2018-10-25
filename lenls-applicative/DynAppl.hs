module DynAppl where

import           DynAppl.Model
import           DynAppl.Assignments
import           Control.Monad.Cont             ( Cont )
import           Control.Lens                   ( (&) ) -- & is backwards application
import           Control.Applicative            ( liftA2 )
import           Data.Functor.Compose           ( Compose ) -- we need this for composition of applicatives. Unfortunately it introduces some boilerplate.

-- C with some additional boilerplate
type CAlt = Compose (Cont (G T)) ((->) Assignment)

-- C without the additional boilerplate (largely equivalent, but will make our lives easier)
newtype C a = C { (>>-) :: (G a -> G T) -> G T }

instance Functor C where
  fmap f cx = C $ \k -> cx >>- \x -> k (fmap f x)

instance Applicative C where
  pure a = C $ \k -> k (pure a)
  cf <*> cx = C $ \k -> cf >>- \f -> cx >>- \x -> k (f <*> x)

-- monad instance for comparison
instance Monad C where
  return = pure
  cgx >>= f = C $ \k -> cgx >>- \gx -> gx >>= (\x -> f x >>- k)

-- We need a version of <*> for when the linear order of function and argument are reversed; we just lift backwards application (&) into the applicative functor we're interested in.
(<\>) :: Applicative f => f a -> f (a -> b) -> f b
(<\>) = liftA2 (&)

-- static someone
someone :: Var -> C E
someone n = C $ \k -> (exFO n) . k $ pro n

liftG :: G a -> C a
liftG x = C $ \k -> k (\g -> x g)

module ParteeRooth where

import Control.Applicative (liftA2, liftA3)

type T = Bool

data Ent = Tom | Dick | Harry deriving (Eq, Show)

type E = Ent

_angry :: E -> T
_angry Tom = True
_angry _ = False

_confident :: E -> T
_confident Tom = True
_confident Dick = True
_confident _ = False

_hug :: E -> E -> T
_hug Tom Dick = True
_hug Dick Tom = True
_hug _ _ = False

_kiss :: E -> E -> T
_kiss Tom Dick = True
_kiss _ _ = False

_and :: T -> T -> T
_and = (&&)

-- liftA2 f x = (<*>) $ fmap f x

_and' :: (a -> T) -> (a -> T) -> a -> T
_and' = liftA2 _and

_and'' :: (a -> b -> T) -> (a -> b -> T) -> a -> b -> T
_and'' = liftA2 _and'

-- >>> ((_confident) `_and'` (_angry)) Tom
-- True

-- >>> ((_hug) `_and''` (_kiss)) Tom $ Dick
-- True

-- applicatives and DPL -- there and back again.
module Dpl where

import Control.Applicative

data E = Tom | Dick | Harry | Empty deriving (Show, Eq, Enum, Ord, Bounded)

univ :: [E]
univ = [(minBound) ..]

type T = Bool

data Var = X | Y | Z deriving (Show, Eq, Enum, Ord, Bounded)

type G = Var -> E

_g1 :: G
_g1 _ = Tom

_g2 :: G
_g2 _ = Dick

_g3 :: G
_g3 _ = Dick

_g4 :: G
_g4 X = Tom
_g4 Y = Dick
_g4 Z = Harry

_g5 :: G
_g5 X = Dick
_g5 Y = Tom
_g5 Z = Harry

_left :: E -> T
_left Tom = True
_left _ = False

_hugs :: E -> E -> T
_hugs Tom Dick = True
_hugs Dick Tom = True
_hugs _ _ = False

modify :: Var -> E -> G -> G
modify v x g = \v' -> if v == v' then x else g v'

ex_static :: Var -> (G -> T) -> G -> T
ex_static v p g = elem True [(p (modify v x g)) | x <- univ]

ex_dyn :: Var -> (G -> G -> T) -> G -> G -> T
ex_dyn v p = \g -> (\h -> (elem True [(p g (modify v x h)) | x <- univ]))

_pro :: Var -> G -> E
_pro v g = g v

_proDyn :: Var -> G -> G -> E
_proDyn v g h = if ((g v) == (h v)) then h v else Empty
-- Need to write a Monoidal instance for the test probably.

-- >>> (ex_static X ((pure _left) <*> (_pro X))) _g1
-- True

-- >>> :t (ex_dyn X (liftA2 (<*>) ((pure . pure) _left) (_proDyn X)))
-- (ex_dyn X (liftA2 (<*>) ((pure . pure) _left) (_proDyn X)))
--   :: G -> G -> T

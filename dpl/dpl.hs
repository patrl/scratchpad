-- DPL with sequences and applicatives!

module Dpl where

import Control.Monad (replicateM)
import Control.Applicative (liftA2)
import Control.Lens

 -- helper functions

toGraph f = [(g, (f g)) | g <- assignments]
toList f = [g | (g,True) <- (toGraph f) ]

toGraph2 f = [(x, (uncurry f $ x)) | x <- (liftA2 (,) assignments assignments )]

toList2 f = [x | (x,True) <- (toGraph2 f) ]

data E = A | B | C | X deriving (Eq, Show, Enum)

type T = Bool

type G = [E]

leave :: E -> T
leave A = True
leave B = True
leave _ = False

hugs :: E -> E -> T
hugs C B = True
hugs A B = True
hugs C C = True
hugs _ _ = False

dom :: [E]
dom = [A .. C]

assignments :: [G]
assignments = [ xs | xs <- (replicateM (length dom) dom) ]

modified xs n = [ xs & element n .~ x  | x <- dom ]

pro :: Int -> G -> E
pro n g = g !! n

ex :: Int -> (G -> T) -> G -> T
ex n p g  = (filter (\h -> p h) (modified g n)) /= []

proDyn :: Int -> G -> G -> E
proDyn n g h = if h == g then h !! n else X

exDyn :: Int -> (G -> G -> T) -> G -> G -> T
exDyn n p g h = (filter (\i -> (p h i)) (modified g n)) /= []

-- >>> toList2 $ exDyn 0 $ (liftA2 (<*>)) ((pure . pure) leave) (proDyn 0)
-- [([A,A,A],[A,A,A]),([A,A,A],[B,A,A]),([A,A,B],[A,A,B]),([A,A,B],[B,A,B]),([A,A,C],[A,A,C]),([A,A,C],[B,A,C]),([A,B,A],[A,B,A]),([A,B,A],[B,B,A]),([A,B,B],[A,B,B]),([A,B,B],[B,B,B]),([A,B,C],[A,B,C]),([A,B,C],[B,B,C]),([A,C,A],[A,C,A]),([A,C,A],[B,C,A]),([A,C,B],[A,C,B]),([A,C,B],[B,C,B]),([A,C,C],[A,C,C]),([A,C,C],[B,C,C]),([B,A,A],[A,A,A]),([B,A,A],[B,A,A]),([B,A,B],[A,A,B]),([B,A,B],[B,A,B]),([B,A,C],[A,A,C]),([B,A,C],[B,A,C]),([B,B,A],[A,B,A]),([B,B,A],[B,B,A]),([B,B,B],[A,B,B]),([B,B,B],[B,B,B]),([B,B,C],[A,B,C]),([B,B,C],[B,B,C]),([B,C,A],[A,C,A]),([B,C,A],[B,C,A]),([B,C,B],[A,C,B]),([B,C,B],[B,C,B]),([B,C,C],[A,C,C]),([B,C,C],[B,C,C]),([C,A,A],[A,A,A]),([C,A,A],[B,A,A]),([C,A,B],[A,A,B]),([C,A,B],[B,A,B]),([C,A,C],[A,A,C]),([C,A,C],[B,A,C]),([C,B,A],[A,B,A]),([C,B,A],[B,B,A]),([C,B,B],[A,B,B]),([C,B,B],[B,B,B]),([C,B,C],[A,B,C]),([C,B,C],[B,B,C]),([C,C,A],[A,C,A]),([C,C,A],[B,C,A]),([C,C,B],[A,C,B]),([C,C,B],[B,C,B]),([C,C,C],[A,C,C]),([C,C,C],[B,C,C])]

-- list of assignments

-- >>> assignments
-- [[A,A,A],[A,A,B],[A,A,C],[A,B,A],[A,B,B],[A,B,C],[A,C,A],[A,C,B],[A,C,C],[B,A,A],[B,A,B],[B,A,C],[B,B,A],[B,B,B],[B,B,C],[B,C,A],[B,C,B],[B,C,C],[C,A,A],[C,A,B],[C,A,C],[C,B,A],[C,B,B],[C,B,C],[C,C,A],[C,C,B],[C,C,C]]

-- free variable example

-- >>> toList ((pure leave) <*> (pro 0))
-- [[A,A,A],[A,A,B],[A,A,C],[A,B,A],[A,B,B],[A,B,C],[A,C,A],[A,C,B],[A,C,C]]

-- >>> toList ((pure leave) <*> (pro 1))
-- [[A,A,A],[A,A,B],[A,A,C],[B,A,A],[B,A,B],[B,A,C],[C,A,A],[C,A,B],[C,A,C]]

-- constant example

-- >>> toList ((pure leave) <*> (pure A))
-- [[A,A,A],[A,A,B],[A,A,C],[A,B,A],[A,B,B],[A,B,C],[A,C,A],[A,C,B],[A,C,C],[B,A,A],[B,A,B],[B,A,C],[B,B,A],[B,B,B],[B,B,C],[B,C,A],[B,C,B],[B,C,C],[C,A,A],[C,A,B],[C,A,C],[C,B,A],[C,B,B],[C,B,C],[C,C,A],[C,C,B],[C,C,C]]


-- existential quantification

-- >>> toList (((pure hugs) <*> (pro 0)) <*> (pro 0))
-- [[C,A,A],[C,A,B],[C,A,C],[C,B,A],[C,B,B],[C,B,C],[C,C,A],[C,C,B],[C,C,C]]

-- >>> toList (ex 0 (((pure hugs) <*> (pro 0)) <*> (pro 0)))
-- [[A,A,A],[A,A,B],[A,A,C],[A,B,A],[A,B,B],[A,B,C],[A,C,A],[A,C,B],[A,C,C],[B,A,A],[B,A,B],[B,A,C],[B,B,A],[B,B,B],[B,B,C],[B,C,A],[B,C,B],[B,C,C],[C,A,A],[C,A,B],[C,A,C],[C,B,A],[C,B,B],[C,B,C],[C,C,A],[C,C,B],[C,C,C]]

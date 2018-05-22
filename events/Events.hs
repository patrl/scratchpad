module Events where

import Control.Lens

data Ent = A | B | C deriving (Eq, Show, Enum)

dom :: [Ent]
dom = [A .. C]

data Manner = LEAVING | LIKING deriving (Eq, Show)

type T = Bool

data V = V {
  manner :: Maybe Manner
  , agent :: Maybe Ent
  , theme :: Maybe Ent
           }

leaves e = manner e == Just LEAVING

likes e = manner e == Just LIKING

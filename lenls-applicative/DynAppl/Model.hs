module DynAppl.Model where

import Control.Monad ( replicateM )

-- remember to cite Dylan for the model

---------------
-- the model --
---------------

-- truth values
type T = Bool

-- entities
newtype E = E (String, Int)

instance Show E where
  show (E (xs,n)) = xs ++ show n

instance Eq E where
  (E ent1) == (E ent2) = ent1 == ent2

instance Ord E where
  compare (E (_,n1)) (E (_,n2)) = compare n1 n2

boys, girls :: [E]
boys = map (\x -> E ("b", x)) [1 .. 2]
girls = map (\x -> E ("g", x)) [1 .. 2]

_b1, _b2 :: E
[_b1, _b2 ] = boys

_g1, _g2 :: E
[_g1, _g2 ] = girls

dom :: [E]
dom = boys ++ girls

_boy :: E -> T
_boy = (`elem` boys)

_girl :: E -> T
_girl = (`elem` girls)

_thing :: E -> T
_thing = const True

_likes, _envies, _pities, _listensTo :: E -> E -> T

-- people like other people when their indices match:
-- b1 likes g1, g3 likes b3, but g5 doesn't like b4 or g4
_likes (E (x, n)) (E (y, m)) = n == m && y /= "p" && x /= "p"

-- people envy people of the same gender that they are less than:
-- b1 envies b3, but b3 does not envy b1 nor does he envy g6
_envies (E (x, n)) (E (y, m)) = x == y && n > m

-- people pity people that envy them:
-- b3 pities b1, but not g1, nor does b1 pity him
_pities (E (x, n)) (E (y, m)) = x == y && n < m

-- people listen to people of the opposite gender that they divide evenly:
-- b2 listens to g6, as does b3, but b4 doesn't, and neither does g2
_listensTo (E (x, n)) (E (y, m)) =
  n `mod` m == 0 && (x == "g" && y == "b" || x == "b" && y == "g")

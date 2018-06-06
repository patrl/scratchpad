-- looks like we need the State monad and the Cont T monad to reconstruct Schlenker's notion of local contexts. It's gonna look EXTREMELY similar to Chierchia's dynamic semantics.
-- TODO: check out Woojin's paper: https://drive.google.com/file/d/0B4lu5n7x39bbOGpMeG5aMjBvc0E

module LocalContexts where

import Control.Monad.Cont
import Control.Monad.Identity
import Control.Monad.State

-- replacing syntactic sugar with a function
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

-- monadic if-then-else
ifF :: Monad m => m Bool -> m a -> m a -> m a
ifF = liftM3 if'

(>>-) = runCont

toCharFunc :: Eq a => [a] -> a -> T
toCharFunc xs x = elem x xs

-- generalized conjunction
andM :: Cont T a -> Cont T a -> Cont T a
andM cf cg = ContT $ \k ->
  Identity $ cf >>- \f ->
                      cg >>- \g ->
                               (runIdentity $ k f) && (runIdentity $ k g)

-- material implication
(==>) :: T -> T -> T
p ==> q = not p || q

-- generalized entailment
(|=) :: Cont T a -> Cont T a -> Cont T a
cf |= cg = ContT $ \k ->
  Identity $ cf >>- \f ->
                      cg >>- \g ->
                               (runIdentity $ k f) ==> (runIdentity $ k g)

type T = Bool

data E = A | B | C deriving (Eq, Show, Enum)

type S = Int

type I a = S -> a

contextSet = [0..]

dom :: [E]
dom = [A .. C]

boy :: E -> S -> T
boy A = toCharFunc [0,2,3,4]
boy B = toCharFunc [0,1,3,5]
boy C = toCharFunc [0,1,2,6]

-- girl and boy are taken to be mutually exclusive here
girl :: E -> S -> T
girl A = toCharFunc [1,5,6,7]
girl B = toCharFunc [2,4,6,7]
girl C = toCharFunc [3,4,5,7]

--
leave :: E -> S -> T
leave A = toCharFunc [0..7]
leave B = toCharFunc []
leave C = toCharFunc [3,4,5,7]

hugs :: E -> E -> S -> T
hugs A A = toCharFunc [0,1]
hugs B B = toCharFunc [0,1]
hugs C C = toCharFunc [0,1]
hugs _ _ = toCharFunc [1]


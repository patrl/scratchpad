-- looks like we need the State monad and the Cont T monad to reconstruct Schlenker's notion of local contexts. It's gonna look EXTREMELY similar to Chierchia's dynamic semantics.
-- TODO: check out Woojin's paper: https://drive.google.com/file/d/0B4lu5n7x39bbOGpMeG5aMjBvc0E

module LocalContexts where

import Control.Monad.Cont
import Control.Monad.Identity
import Control.Monad.Reader

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

-- intensional generalized conjunction
andS :: Cont (S -> T) a -> Cont (S -> T) a -> Cont (S -> T) a
andS cf cg = ContT $ \k ->
  Identity $ cf >>- \f ->
                      cg >>- \g ->
                               (liftM2 (&&)) (runIdentity $ k f) (runIdentity $ k g)

-- conjoin the terminating value of a predicate with the local context.
andHelper :: Cont (S -> T) a -> (S -> T) -> Cont (S -> T) a
andHelper cf p = ContT $ \k ->
  Identity $ cf >>- \f ->
                      (liftM2 (&&)) (runIdentity $ k f) p

entailsHelper :: Cont (S -> T) (S -> T) -> Cont (S -> T) a -> Cont (S -> T) a
entailsHelper cp cf = ContT $ \k ->
  Identity $ cf >>- \f ->
  cp >>- \p ->
  (liftM2 (==>)) p (runIdentity $ k f)

andHelper' :: Cont (S -> T) a -> Cont (S -> T) (S -> T) -> Cont (S -> T) a
andHelper' cf cp = ContT $ \k ->
  Identity $ cf >>- \f ->
  cp >>- \p ->
  (liftM2 (&&)) (runIdentity $ k f) p

-- material implication
(==>) :: T -> T -> T
p ==> q = not p || q

-- generalized entailment
(|=) :: Cont (S -> T) a -> Cont (S -> T) a -> Cont (S -> T) a
cf |= cg = ContT $ \k ->
  Identity $ cf >>- \f ->
                      cg >>- \g ->
                               (liftM2 (==>)) (runIdentity $ k f) (runIdentity $ k g)

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

newtype LC a = LC { runLC :: Cont (S -> T) (S -> T) -> Cont (S -> T) a }

universe :: Cont (S -> T) (S -> T)
universe = (return :: a -> Cont (S -> T) a) $ toCharFunc [0..]

-- More generally: A monad for checking that the value of the local environment is compatible with the terminating value of the computation. Almost identical to the Chierchia monad.

instance Functor LC where
  fmap f (LC a) = LC $ \p ->
                         fmap f (a p)
                         `andHelper'` p

instance Applicative LC where
  pure a = LC $ \p ->
                  (pure a) `andHelper'` p
  (LC f) <*> (LC x) = LC $ \p ->
                             (f p) <*> (x p)
                             `andHelper'` p

instance Monad LC where
  return = pure
  (LC x) >>= f = LC $ \p ->
    ((x p) >>= (\y ->
                 ($ p) . runLC $ f y)) `andHelper'` p

-- >>> toSet [0..10] . lower . ($ (return $ toCharFunc [0..10])) . runLC $ (return :: a -> LC a) ((liftM2 (&&)) ((return A) >>= leave) ((return A) >>= boy))
-- [0,2,3,4]

-- update
(+++) :: LC (S -> T) -> LC (S -> T) -> LC (S -> T)
(LC p) +++ (LC q) = LC $ \r ->
                          p . q $ r

-- >>> toSet [0..] . lower . ($ universe) . runLC $ (return ((return A) >>= leave)) +++ (return ((return A) >>= boy))
-- [0,2,3,4]



  -- let's try something presuppositional. leave presupposes now that A is a boy.
leavePresupp :: LC (E -> S -> T)
leavePresupp = LC $ \p ->
                      ContT $ \k ->
                                k (\x -> (leave x))

  
                      -- ifF (p `entailsHelper` (return $ boy)) (return leave) (return (\x -> (\w -> False)))

-- >>> 1 + 1
-- 2

-- >>> toSet [0..10] . lower . ($ universe) . runLC $ leavePresupp `ap` (return A)
-- >>> presupposition narrows down the context in exactly the way we predict.
-- [0,2,3,4]
 
-- lowers a continuationized proposition
lower :: Cont a a -> a
lower cp = runCont cp id

toSet :: [a] -> (a -> T) -> [a]
toSet dom f = [x | (x,True) <-
                   toGraph dom f]

-- a function from a predicate and a domain, to the graph of the predicate
toGraph :: [a] -> (a -> T) -> [(a,T)]
toGraph dom f = [(x, (f x)) | x <- dom]





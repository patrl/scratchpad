-- Exploring the links between an account of context-sensitivity based on applicative functors (Charlow 2018), and basic dynamic theories such as DPL.

module Dpl where

import Control.Monad (replicateM, zipWithM_)
import Control.Applicative (liftA2)
import Control.Lens (set, element)
import Data.Functor.Compose

-- model theory

-- X is the impossible individual
data E = John | Mary | Bill | X deriving (Eq, Show, Enum)

type T = Bool

leave :: E -> T
leave John = True
leave _ = False

hugs :: E -> E -> T
hugs Bill Mary = True
hugs John Mary = True
hugs Bill Bill = True
hugs _ _ = False

_and :: T -> T -> T
_and = (&&)

_or :: T -> T -> T
_or = (||)

dom :: [E]
dom = [John .. Bill]

-- the set of possible assignments are all possible lists of entities of length 3.
assignments :: [C]
assignments = [ xs | xs <- (replicateM (length dom) dom) ]

-- A (static) context is just a list of entities.
type C = [E]

-- G is a type-constructor to static context-sensitive meanings.
newtype G a =  G (C -> a)

-- The functor instance for G is just the functor instance for ((->) C) wrapped in G.
instance Functor G where
  fmap f (G a) = (G $ fmap f a)

-- The applicative instance for G is just the applicative instance for ((->) C) wrapped in G.
instance Applicative G where
  pure x = (G (\g -> x))
  (G f) <*> (G x) = (G $ f <*> x)

-- A helper function to get rid of the newtype wrapper.
fromG :: G a -> (C -> a)
fromG (G x) = x

-- This is a helper function that takes a context g, and returns the set of all assignment functions g' which differ at most from g in the nth element.
modified g n = [ set (element n) x g  | x <- dom ]

-- Static pronouns are functions from an index n and a context g, to the nth element of g.
pro :: Int -> G E
pro n = G (\g -> (g !! n))

-- Static existential quantification is a function from an index n, and a context-sensitive proposition p, to (the characteristic function of) the set of contexts g, s.t. there is a context h, which differs at most from g in the nth element, and which makes p true.
ex :: Int -> G T -> G T
ex n (G p) = G (\g -> (not . null $ filter (\h -> (p h)) (modified g n)))

forall :: Int -> G T -> G T
forall n (G p) = G (\g -> (all (\h -> (p h)) (modified g n)))

-- In a context-sensitive setting, booleans connectives are lifted.
_andAppl :: G T -> G T -> G T
_andAppl = liftA2 _and

_orAppl :: G T -> G T -> G T
_orAppl = liftA2 _or

-- The type constructor for dynamic context-sensitive meanings is the composition of G with itself, which is a wrapper around the composition of ((->) C) with itself. This is guaranteed to be an applicative functor.
type CC a = Compose G G a

-- helper function to deal with all the annoying newtype wrappers introduced by composition of applicatives.
satCC :: CC T -> C -> C -> T
satCC p i o = (($ o) . fromG . ($ i) . fromG . getCompose) p

-- Dynamic pronouns are tests:
-- functions from an index n, to a function from an input/output (i,o) to the nth individual in o if i and o are the same, else the impossible individual.
proDyn :: Int -> CC E
proDyn n = Compose (G (\i -> (G (\o -> ( if i == o
                                       then o !! n
                                       else X )))))

-- dynamic existential quantification is a function from an index n, and a set of inputs/outputs p, to a set of inputs/outputs (i,o), such that there is a context g' which differs from i only in the nth element and (o,g') is in p.
exDyn :: Int -> CC T -> CC T
exDyn n p = Compose (G (\i ->
                          (G (\o ->
                                ( not . null $
                                  filter
                                  (\g -> (satCC p o g))
                                  (modified i n))))))

-- Dynamic negation is a function from a set of inputs/outputs p, to a set of inputs/outputs (i,o), s.t. i and o are identical, and there is no (o,g') in p (for any context g').
negDyn :: CC T -> CC T
negDyn p = Compose (G (\i ->
                         (G (\o ->
                               ( o == i
                                 && null [ g | g <- assignments, satCC p o g])))))

dynImplic :: CC T -> CC T -> CC T
dynImplic p q = Compose (G (\i ->
                              (G (\o ->
                                    ( o == i
                                      && (all
                                          (\g ->
                                             (not . null $
                                              [h | h <- assignments, satCC p g h]))
                                          [g | g <- assignments, satCC p o g]))))))

dynConj :: CC T -> CC T -> CC T
dynConj p q = Compose (G (\i -> (G (\o -> ((not . null) [g | g <- assignments, satCC p i g, satCC q g o ])))))

-- N.b. we can use (liftA2 (&&)) in a dynamic setting to get conjunction that is both internally and externally static.

dynDisj :: CC T -> CC T -> CC T
dynDisj p q = Compose (G (\i -> (G (\o -> ( o == i
                                            && (not . null) [g | g <- assignments, satCC p o g || satCC q o g])))))

dynUniv :: Int -> CC T -> CC T
dynUniv n p = Compose (G (\o -> (G (\i -> ( o == i
                                            && (all
                                                (\g -> ((not . null) [h | h <- assignments, satCC p g h]))
                                                (modified o n)))))))

 -- Functions for pretty-printing contexts.

toGraph :: G T -> [(C,T)]
toGraph (G f) = [(g, (f g)) | g <- assignments]

toList :: G T -> [C]
toList f = [g | (g,True) <- (toGraph f) ]

toGraph2 :: CC T -> [((C,C),T)]
toGraph2 f = [(x, (uncurry (satCC f) $ x)) | x <- (liftA2 (,) assignments assignments )]

toList2 :: CC T -> [(C,C)]
toList2 f = [x | (x,True) <- (toGraph2 f) ]

prettyPrintAssignment :: C -> IO ()
prettyPrintAssignment xs = putStrLn $ ((unwords . (map show)) xs)

prettyPrintStaticContext :: G T -> IO ()
prettyPrintStaticContext xs = mapM_ prettyPrintAssignment (toList xs)

prettyPrintCC :: (C,C) -> IO ()
prettyPrintCC (i,o) = putStrLn $ ((unwords . (map show)) i) ++ " -> " ++ ((unwords . (map show)) o)

prettyPrintDynContext :: CC T -> IO ()
prettyPrintDynContext xs = mapM_ prettyPrintCC (toList2 xs)

-- Everyone left

-- >>> prettyPrintDynContext $ dynUniv 0 $ (pure leave) <*> (proDyn 0)

-- Someone left

-- >>> prettyPrintDynContext $ exDyn 0 $ (pure leave) <*> (proDyn 0)
-- John John John -> John John John
-- John John Mary -> John John Mary
-- John John Bill -> John John Bill
-- John Mary John -> John Mary John
-- John Mary Mary -> John Mary Mary
-- John Mary Bill -> John Mary Bill
-- John Bill John -> John Bill John
-- John Bill Mary -> John Bill Mary
-- John Bill Bill -> John Bill Bill
-- Mary John John -> John John John
-- Mary John Mary -> John John Mary
-- Mary John Bill -> John John Bill
-- Mary Mary John -> John Mary John
-- Mary Mary Mary -> John Mary Mary
-- Mary Mary Bill -> John Mary Bill
-- Mary Bill John -> John Bill John
-- Mary Bill Mary -> John Bill Mary
-- Mary Bill Bill -> John Bill Bill
-- Bill John John -> John John John
-- Bill John Mary -> John John Mary
-- Bill John Bill -> John John Bill
-- Bill Mary John -> John Mary John
-- Bill Mary Mary -> John Mary Mary
-- Bill Mary Bill -> John Mary Bill
-- Bill Bill John -> John Bill John
-- Bill Bill Mary -> John Bill Mary
-- Bill Bill Bill -> John Bill Bill

-- they_0 didn't leave

-- >>> prettyPrintDynContext $ negDyn $ (pure leave) <*> (proDyn 0)
-- Mary John John -> Mary John John
-- Mary John Mary -> Mary John Mary
-- Mary John Bill -> Mary John Bill
-- Mary Mary John -> Mary Mary John
-- Mary Mary Mary -> Mary Mary Mary
-- Mary Mary Bill -> Mary Mary Bill
-- Mary Bill John -> Mary Bill John
-- Mary Bill Mary -> Mary Bill Mary
-- Mary Bill Bill -> Mary Bill Bill
-- Bill John John -> Bill John John
-- Bill John Mary -> Bill John Mary
-- Bill John Bill -> Bill John Bill
-- Bill Mary John -> Bill Mary John
-- Bill Mary Mary -> Bill Mary Mary
-- Bill Mary Bill -> Bill Mary Bill
-- Bill Bill John -> Bill Bill John
-- Bill Bill Mary -> Bill Bill Mary
-- Bill Bill Bill -> Bill Bill Bill

-- basic donkey
-- Someone_0 left and Mary hugged them_0

-- >>> prettyPrintDynContext $ (exDyn 0 $ (pure leave) <*> (proDyn 0)) `dynConj` (((pure hugs) <*> (proDyn 0)) <*> (pure Mary))
-- John John John -> John John John
-- John John Mary -> John John Mary
-- John John Bill -> John John Bill
-- John Mary John -> John Mary John
-- John Mary Mary -> John Mary Mary
-- John Mary Bill -> John Mary Bill
-- John Bill John -> John Bill John
-- John Bill Mary -> John Bill Mary
-- John Bill Bill -> John Bill Bill
-- Mary John John -> John John John
-- Mary John Mary -> John John Mary
-- Mary John Bill -> John John Bill
-- Mary Mary John -> John Mary John
-- Mary Mary Mary -> John Mary Mary
-- Mary Mary Bill -> John Mary Bill
-- Mary Bill John -> John Bill John
-- Mary Bill Mary -> John Bill Mary
-- Mary Bill Bill -> John Bill Bill
-- Bill John John -> John John John
-- Bill John Mary -> John John Mary
-- Bill John Bill -> John John Bill
-- Bill Mary John -> John Mary John
-- Bill Mary Mary -> John Mary Mary
-- Bill Mary Bill -> John Mary Bill
-- Bill Bill John -> John Bill John
-- Bill Bill Mary -> John Bill Mary
-- Bill Bill Bill -> John Bill Bill

-- scopal commutativity
-- Someone_0 is s.t. they_0 left and Mary hugged them_0

-- >>> prettyPrintDynContext $ exDyn 0 $ ((pure leave) <*> (proDyn 0)) `dynConj` (((pure hugs) <*> (proDyn 0)) <*> (pure Mary))
-- John John John -> John John John
-- John John Mary -> John John Mary
-- John John Bill -> John John Bill
-- John Mary John -> John Mary John
-- John Mary Mary -> John Mary Mary
-- John Mary Bill -> John Mary Bill
-- John Bill John -> John Bill John
-- John Bill Mary -> John Bill Mary
-- John Bill Bill -> John Bill Bill
-- Mary John John -> John John John
-- Mary John Mary -> John John Mary
-- Mary John Bill -> John John Bill
-- Mary Mary John -> John Mary John
-- Mary Mary Mary -> John Mary Mary
-- Mary Mary Bill -> John Mary Bill
-- Mary Bill John -> John Bill John
-- Mary Bill Mary -> John Bill Mary
-- Mary Bill Bill -> John Bill Bill
-- Bill John John -> John John John
-- Bill John Mary -> John John Mary
-- Bill John Bill -> John John Bill
-- Bill Mary John -> John Mary John
-- Bill Mary Mary -> John Mary Mary
-- Bill Mary Bill -> John Mary Bill
-- Bill Bill John -> John Bill John
-- Bill Bill Mary -> John Bill Mary
-- Bill Bill Bill -> John Bill Bill

-- A type constructor for tests.

newtype CCAlt a = CCAlt (C -> C -> Maybe a)

instance Functor CCAlt where
  fmap f (CCAlt x) = CCAlt (\i -> (\o -> (fmap f (x i o))))

instance Applicative CCAlt where
  pure a = CCAlt (\i -> (\o -> (if o == i then Just a else Nothing)))
  (CCAlt f) <*> (CCAlt x) = CCAlt (\i -> (\o -> (if i == o then (f i o) <*> (x i o) else Nothing)))


proDynAlt :: Int -> CCAlt E
proDynAlt n = CCAlt ((\i -> (\o -> if o == i then Just (o !! n) else Nothing)))

-- these two entries bring out the similarities

-- The output context of saturates the output context of the pronoun, and Maybe wraps the entity value

proStat' :: Int -> C -> E
proStat' n o = o !! n

proStat1 :: C -> E
proStat1 = proStat' 1

pureContext = (pure :: a -> (C -> a))

applContext = ((<*>) :: (C -> a -> b) -> (C -> a) -> (C -> b))

proStat1ToDyn :: (C -> E) -> C -> C -> Maybe E
proStat1ToDyn pro i o = if i == o then Just (pro o) else Nothing

testPure :: (C -> a) -> C -> C -> Maybe a
testPure x i o = if i == o then Just (x o) else Nothing

testAppl :: (C -> (a -> b)) -> (C -> a) -> C -> C -> Maybe b
testAppl f x i o = if i == o then Just ((f o) (x o)) else Nothing

-- need to work a bit more on generalizing this

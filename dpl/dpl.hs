-- applicative DPL

module Dpl where

import Control.Monad (replicateM, zipWithM_)
import Control.Applicative (liftA2)
import Control.Lens

 -- Functions for pretty-printing contexts.

toGraph f = [(g, (f g)) | g <- assignments]

toList f = [g | (g,True) <- (toGraph f) ]

toGraph2 f = [(x, (uncurry f $ x)) | x <- (liftA2 (,) assignments assignments )]

toList2 f = [x | (x,True) <- (toGraph2 f) ]

prettyPrintAssignment :: [E] -> IO ()
prettyPrintAssignment xs = putStrLn $ ((unwords . (map show)) xs)

prettyPrintStaticContext :: G T -> IO ()
prettyPrintStaticContext xs = mapM_ prettyPrintAssignment (toList xs)

prettyPrintCC :: ([E],[E]) -> IO ()
prettyPrintCC (i,o) = putStrLn $ ((unwords . (map show)) i) ++ " -> " ++ ((unwords . (map show)) o)

prettyPrintDynContext :: CC T -> IO ()
prettyPrintDynContext xs = mapM_ prettyPrintCC (toList2 xs)

-- model theory

-- X is the impossible individual
data E = A | B | C | X deriving (Eq, Show, Enum)

type T = Bool

type G a = [E] -> a

type CC a = G ( G a )

leave :: E -> T
leave A = True
leave _ = False

hugs :: E -> E -> T
hugs C B = True
hugs A B = True
hugs C C = True
hugs _ _ = False

dom :: [E]
dom = [A .. C]

assignments :: [[E]]
assignments = [ xs | xs <- (replicateM (length dom) dom) ]

modified xs n = [ xs & element n .~ x  | x <- dom ]

pro :: Int -> G E
pro n g = g !! n

ex :: Int -> G T -> G T
ex n p g  = not . null $ filter (\h -> p h) (modified g n)

-- pronouns as tests
proDyn :: Int -> CC E
proDyn n i o = if i == o
               then o !! n
               else X

exDyn :: Int -> CC T -> CC T
exDyn n p i o = not . null $ filter (\g -> (p o g)) (modified i n)

forallDyn :: Int -> CC T -> CC T
forallDyn = undefined

negDyn :: CC T -> CC T
negDyn p i o = o == i
               && null [ g | g <- assignments, p o g]

dynConj :: CC T -> CC T -> CC T
dynConj p q i o = (not . null) [g | g <- assignments, p i g, q g o ]

dynDisj :: CC T -> CC T -> CC T
dynDisj p q i o = o == i
                  && (not . null) [g | g <- assignments, p o g || q o g]

dynUniv :: Int -> CC T -> CC T
dynUniv n p i o = o == i
                  && (all
                      (\g -> ((not . null) [h | h <- assignments, p g h]))
                      (modified o n))

-- >>> prettyPrintDynContext $ dynUniv 0 $ ((liftA2 (<*>)) (pure . pure $ leave) (proDyn 0))

-- negation

-- >>> prettyPrintDynContext $ negDyn (((liftA2 (<*>)) (pure . pure $ leave) (proDyn 0)))
-- B A A -> B A A
-- B A B -> B A B
-- B A C -> B A C
-- B B A -> B B A
-- B B B -> B B B
-- B B C -> B B C
-- B C A -> B C A
-- B C B -> B C B
-- B C C -> B C C
-- C A A -> C A A
-- C A B -> C A B
-- C A C -> C A C
-- C B A -> C B A
-- C B B -> C B B
-- C B C -> C B C
-- C C A -> C C A
-- C C B -> C C B
-- C C C -> C C C

-- >>> prettyPrintDynContext $ exDyn 0 $ (liftA2 (<*>)) ((pure . pure) leave) (proDyn 0)
-- A A A -> A A A
-- A A A -> B A A
-- A A A -> C A A
-- A A B -> A A B
-- A A B -> B A B
-- A A B -> C A B
-- A A C -> A A C
-- A A C -> B A C
-- A A C -> C A C
-- A B A -> A B A
-- A B A -> B B A
-- A B A -> C B A
-- A B B -> A B B
-- A B B -> B B B
-- A B B -> C B B
-- A B C -> A B C
-- A B C -> B B C
-- A B C -> C B C
-- A C A -> A C A
-- A C A -> B C A
-- A C A -> C C A
-- A C B -> A C B
-- A C B -> B C B
-- A C B -> C C B
-- A C C -> A C C
-- A C C -> B C C
-- A C C -> C C C
-- B A A -> A A A
-- B A A -> B A A
-- B A A -> C A A
-- B A B -> A A B
-- B A B -> B A B
-- B A B -> C A B
-- B A C -> A A C
-- B A C -> B A C
-- B A C -> C A C
-- B B A -> A B A
-- B B A -> B B A
-- B B A -> C B A
-- B B B -> A B B
-- B B B -> B B B
-- B B B -> C B B
-- B B C -> A B C
-- B B C -> B B C
-- B B C -> C B C
-- B C A -> A C A
-- B C A -> B C A
-- B C A -> C C A
-- B C B -> A C B
-- B C B -> B C B
-- B C B -> C C B
-- B C C -> A C C
-- B C C -> B C C
-- B C C -> C C C
-- C A A -> A A A
-- C A A -> B A A
-- C A A -> C A A
-- C A B -> A A B
-- C A B -> B A B
-- C A B -> C A B
-- C A C -> A A C
-- C A C -> B A C
-- C A C -> C A C
-- C B A -> A B A
-- C B A -> B B A
-- C B A -> C B A
-- C B B -> A B B
-- C B B -> B B B
-- C B B -> C B B
-- C B C -> A B C
-- C B C -> B B C
-- C B C -> C B C
-- C C A -> A C A
-- C C A -> B C A
-- C C A -> C C A
-- C C B -> A C B
-- C C B -> B C B
-- C C B -> C C B
-- C C C -> A C C
-- C C C -> B C C
-- C C C -> C C C

-- basic donkey

-- >>> prettyPrintDynContext $ (exDyn 0 $ (liftA2 (<*>)) ((pure . pure) leave) (proDyn 0)) `dynConj` ((liftA2 (<*>)) ((liftA2 (<*>)) ((pure . pure) hugs) (proDyn 0)) ((pure . pure) B))
-- A A A -> A A A
-- A A A -> C A A
-- A A B -> A A B
-- A A B -> C A B
-- A A C -> A A C
-- A A C -> C A C
-- A B A -> A B A
-- A B A -> C B A
-- A B B -> A B B
-- A B B -> C B B
-- A B C -> A B C
-- A B C -> C B C
-- A C A -> A C A
-- A C A -> C C A
-- A C B -> A C B
-- A C B -> C C B
-- A C C -> A C C
-- A C C -> C C C
-- B A A -> A A A
-- B A A -> C A A
-- B A B -> A A B
-- B A B -> C A B
-- B A C -> A A C
-- B A C -> C A C
-- B B A -> A B A
-- B B A -> C B A
-- B B B -> A B B
-- B B B -> C B B
-- B B C -> A B C
-- B B C -> C B C
-- B C A -> A C A
-- B C A -> C C A
-- B C B -> A C B
-- B C B -> C C B
-- B C C -> A C C
-- B C C -> C C C
-- C A A -> A A A
-- C A A -> C A A
-- C A B -> A A B
-- C A B -> C A B
-- C A C -> A A C
-- C A C -> C A C
-- C B A -> A B A
-- C B A -> C B A
-- C B B -> A B B
-- C B B -> C B B
-- C B C -> A B C
-- C B C -> C B C
-- C C A -> A C A
-- C C A -> C C A
-- C C B -> A C B
-- C C B -> C C B
-- C C C -> A C C
-- C C C -> C C C

-- >>> prettyPrintDynContext $ exDyn 0 $ ((liftA2 (<*>)) ((pure . pure) leave) (proDyn 0)) `dynConj` ((liftA2 (<*>)) ((liftA2 (<*>)) ((pure . pure) hugs) (proDyn 0)) ((pure . pure) B))
-- A A A -> A A A
-- A A A -> C A A
-- A A B -> A A B
-- A A B -> C A B
-- A A C -> A A C
-- A A C -> C A C
-- A B A -> A B A
-- A B A -> C B A
-- A B B -> A B B
-- A B B -> C B B
-- A B C -> A B C
-- A B C -> C B C
-- A C A -> A C A
-- A C A -> C C A
-- A C B -> A C B
-- A C B -> C C B
-- A C C -> A C C
-- A C C -> C C C
-- B A A -> A A A
-- B A A -> C A A
-- B A B -> A A B
-- B A B -> C A B
-- B A C -> A A C
-- B A C -> C A C
-- B B A -> A B A
-- B B A -> C B A
-- B B B -> A B B
-- B B B -> C B B
-- B B C -> A B C
-- B B C -> C B C
-- B C A -> A C A
-- B C A -> C C A
-- B C B -> A C B
-- B C B -> C C B
-- B C C -> A C C
-- B C C -> C C C
-- C A A -> A A A
-- C A A -> C A A
-- C A B -> A A B
-- C A B -> C A B
-- C A C -> A A C
-- C A C -> C A C
-- C B A -> A B A
-- C B A -> C B A
-- C B B -> A B B
-- C B B -> C B B
-- C B C -> A B C
-- C B C -> C B C
-- C C A -> A C A
-- C C A -> C C A
-- C C B -> A C B
-- C C B -> C C B
-- C C C -> A C C
-- C C C -> C C C


-- list of assignments

-- free variable example

-- >>> prettyPrintStaticContext ((pure leave) <*> (pro 0))
-- A A A
-- A A B
-- A A C
-- A B A
-- A B B
-- A B C
-- A C A
-- A C B
-- A C C
-- B A A
-- B A B
-- B A C
-- B B A
-- B B B
-- B B C
-- B C A
-- B C B
-- B C C

-- >>> prettyPrintStaticContext ((pure leave) <*> (pro 1))
-- A A A
-- A A B
-- A A C
-- A B A
-- A B B
-- A B C
-- B A A
-- B A B
-- B A C
-- B B A
-- B B B
-- B B C
-- C A A
-- C A B
-- C A C
-- C B A
-- C B B
-- C B C

-- constant example

-- >>> prettyPrintStaticContext ((pure leave) <*> (pure A))
-- A A A
-- A A B
-- A A C
-- A B A
-- A B B
-- A B C
-- A C A
-- A C B
-- A C C
-- B A A
-- B A B
-- B A C
-- B B A
-- B B B
-- B B C
-- B C A
-- B C B
-- B C C
-- C A A
-- C A B
-- C A C
-- C B A
-- C B B
-- C B C
-- C C A
-- C C B
-- C C C


-- existential quantification

-- >>> prettyPrintStaticContext (((pure hugs) <*> (pro 0)) <*> (pro 0))
-- C A A
-- C A B
-- C A C
-- C B A
-- C B B
-- C B C
-- C C A
-- C C B
-- C C C

-- >>> prettyPrintStaticContext $ (ex 0 (((pure hugs) <*> (pro 0)) <*> (pro 0)))
-- A A A
-- A A B
-- A A C
-- A B A
-- A B B
-- A B C
-- A C A
-- A C B
-- A C C
-- B A A
-- B A B
-- B A C
-- B B A
-- B B B
-- B B C
-- B C A
-- B C B
-- B C C
-- C A A
-- C A B
-- C A C
-- C B A
-- C B B
-- C B C
-- C C A
-- C C B
-- C C C

-- >>> mapM_ f assignments
-- A A A
-- A A B
-- A A C
-- A B A
-- A B B
-- A B C
-- A C A
-- A C B
-- A C C
-- B A A
-- B A B
-- B A C
-- B B A
-- B B B
-- B B C
-- B C A
-- B C B
-- B C C
-- C A A
-- C A B
-- C A C
-- C B A
-- C B B
-- C B C
-- C C A
-- C C B
-- C C C

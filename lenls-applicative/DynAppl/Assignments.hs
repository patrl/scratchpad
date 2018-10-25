 -- implements parts of Charlow (2018) "A modular theory of pronouns and binding"
 module DynAppl.Assignments where

 import           DynAppl.Model
 import           Control.Monad                  ( replicateM )
 import           Control.Lens                   ( set
                                                 , element
                                                 )

 -- Assignments are treated here as lists of individuals. This will make our lives easier in various ways.
 newtype Assignment = Assignment [E]

 -- Pretty-prints assignments
 -- TODO use prettyprint for this
 instance Show Assignment where
   show (Assignment xs) = ((unwords . map show) xs) ++ "\n"

 _a1 :: Assignment
 _a1 = Assignment [ E ("b", 1), E ("b", 2), E ("b", 3)]
 _a2 :: Assignment
 _a2 = Assignment [ E ("g", 1), E ("g", 2), E ("g", 3)]

 type Var = Int

 -- given a domain of size n, this computes all possible *total* assignments of length n
 assignments :: [Assignment]
 assignments = map Assignment $ replicateM (length dom) dom

 -- helper function for modified assignments
 -- takes an assignment g, an index i, and an entity x and sets the ith element of g to x
 modify :: Assignment -> Var -> E -> Assignment
 modify (Assignment g) i x = Assignment $ set (element i) x g

 -- takes an assignment g, and an index i, and returns the gs that differ from g at most in the ith element (useful for first-order stuff)
 modified :: Assignment -> Var -> [Assignment]
 modified g n = [ modify g n x | x <- dom ]

 -- G is the type-constructor for assignment-sensitive meanings. Since G is just ((->) Assignment), it has Functor and Applicative instances (it's just reader).
 type G a = Assignment -> a

 -- pronominals are functions from an index i and an assignment g to the ith member of g,
 pro :: Var -> G E
 pro i (Assignment g) = g !! i

 -- categorematic abstraction: takes a binding index i, a scope f, and returns an assignment sensitive predicate abstracting over the i.
 abs :: Var -> G a -> G (E -> a)
 abs i f g = f . modify g i

 -- first-order existential quantification
 exFO :: Var -> G T -> G T
 exFO n p g = not . null $ filter p (modified g n)

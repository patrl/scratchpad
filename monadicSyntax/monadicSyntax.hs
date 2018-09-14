module MonadicSyntax where

type T = Bool

data E = A | B | C

-- data SemType = Sem1 E | Sem2 T | Sem3 (SemType -> SemType)

data Cat = N | V | D | T

_boy :: E -> T
_boy A = True
_boy _ = False

john :: (Cat, E)
john = (D, A)

leave :: (Cat -> Maybe Cat, E -> T)
leave = (verb, _leave) where
  verb D = Just V
  verb _ = Nothing

_leave :: E -> T
_leave A = True
_leave B = True
_leave C = False

 -- >>> leave <*> john
-- <interactive>:838:12: error:
--     • Couldn't match type ‘Cat’ with ‘Cat -> Maybe Cat’
--       Expected type: (Cat -> Maybe Cat, E)
--         Actual type: (Cat, E)
--     • In the second argument of ‘(<*>)’, namely ‘john’
--       In the expression: leave <*> john
--       In an equation for ‘it’: it = leave <*> john



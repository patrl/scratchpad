module DynAppl where

import DynAppl.Model
import DynAppl.Assignments
import Control.Monad ( replicateM )

-- >>> (filter ((return _boy) <*> (pro 1)) assignments)

{-# LANGUAGE GADTs, RankNTypes #-}

module Focus where

boy :: E -> T
boy A = True
boy _ = False

girl :: E-> T
girl A = False
girl _ = True

leave :: E -> T
leave A = True
leave _ = False

data E = A | B | C deriving (Eq, Show, Enum, Bounded)

wh :: (Bounded a, Enum a) => [a]
wh = [minBound .. ]


type T = Bool

data Pair m a = Pair { fstPair :: a
                     , sndPair :: m a } deriving (Eq, Show)

fromPair :: F a -> (a,[a])
fromPair (Pair { fstPair = x, sndPair = xs}) = (x,xs)

instance Functor (Pair m) where
  fmap = undefined

instance Applicative m => Applicative (Pair m) where
  pure x = Pair { fstPair = x,
                  sndPair = pure x }
  (Pair {fstPair = f1, sndPair = f2}) <*> (Pair {fstPair = x1, sndPair = x2}) =
    (Pair {fstPair = (f1 x1), sndPair = (f2 <*> x2)})

instance Monad m => Monad (Pair m) where
  return = pure
  (Pair {fstPair = x1,
         sndPair = x2})
    >>= n = Pair { fstPair = fstPair (n x1) , sndPair = x2 >>= (\y ->
                                                          sndPair (n y))}
-- instance Monad Alt where
  -- return a = Alt $ \b -> (b == a)
  -- (>>=) = undefined
  -- (>>=) = undefined

type F = Pair []

foc :: (Eq a, Enum a, Bounded a) => a -> F a
foc x = Pair {fstPair = x, sndPair = filter (`notElem` [x]) wh}

-- >>> only $ (foc A) >>= (\x -> (return $ leave x))
-- Just True

only :: F T -> Maybe T
only (Pair { fstPair = x, sndPair = alt })
  = if not x
    then Nothing
    else
      if (not . or) alt
      then Just True
      else Just False

-- newtype Alt a = Alt { alt :: a -> T }

-- instance Functor Alt where
  -- fmap f (Alt g) = Alt h where
    -- if h = undefined

{-# LANGUAGE FlexibleContexts #-}

module CharacteristicFunctions where

import Model

import Control.Applicative

-- TODO: generalize

-- from https://wiki.haskell.org/Compose
-- compose :: [a -> a] -> a -> a
-- compose fs v = foldl (flip (.)) id fs $ v

toGraph1 f = [(x,(f x)) | x <- [(minBound) ..]]

charFunc1 f = map fst (toGraph1 f)

toGraph2 f = [(x,(uncurry f $ x)) | x <- (liftA2 (,) [(minBound) ..] [(minBound) ..] )]

charFunc2 f = map fst (toGraph2 f)

toGraph3 f = [(x, ((uncurry . uncurry) f $ x)) | x <- (liftA2 (,) (liftA2 (,) [(minBound) ..] [(minBound) ..]) [(minBound) ..])]

charFunc3 f = map fst (toGraph3 f)

toGraph4 f = [(x, ((uncurry . uncurry . uncurry) f $ x)) | x <- (liftA2 (,) (liftA2 (,) (liftA2 (,) [(minBound) ..] [(minBound) ..]) [(minBound) ..]) [(minBound) ..])]

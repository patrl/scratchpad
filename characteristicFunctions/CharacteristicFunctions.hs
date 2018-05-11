{-# LANGUAGE FlexibleContexts #-}

module CharacteristicFunctions where

import Control.Applicative

-- TODO: generalize

toGraph1 f = [(x, (f x)) | x <- [(minBound) ..]]

charFunc1 f = [x | (x,True) <- (toGraph1 f) ]

toGraph2 f = [(x, (uncurry f $ x)) | x <- (liftA2 (,) [(minBound) ..] [(minBound) ..] )]

charFunc2 f = [x | (x,True) <- (toGraph2 f) ]

toGraph3 f = [(x, ((uncurry . uncurry) f $ x)) | x <- (liftA2 (,) (liftA2 (,) [(minBound) ..] [(minBound) ..]) [(minBound) ..])]

charFunc3 f = [x | (x,True) <- (toGraph3 f) ]

toGraph4 f = [(x, ((uncurry . uncurry . uncurry) f $ x)) | x <- (liftA2 (,) (liftA2 (,) (liftA2 (,) [(minBound) ..] [(minBound) ..]) [(minBound) ..]) [(minBound) ..])]

charFunc4 f = [x | (x,True) <- (toGraph4 f) ]

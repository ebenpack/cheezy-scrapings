{-# LANGUAGE TemplateHaskell #-}

module Time where

import Prelude
import Polysemy
import Polysemy.Input

import Data.Time.Clock

data Time m a where
    GetTime :: Time m UTCTime

makeSem ''Time

interpretTimeIO :: Member (Final IO) r => Sem (Time ': r) a -> Sem r a
interpretTimeIO = 
    interpret 
        (\case
            GetTime -> embedFinal $ getCurrentTime)

interpretTimePure :: Members '[Input UTCTime] r => [UTCTime] -> Sem (Time ': r) a -> Sem r a
interpretTimePure i =
    runInputList i . reinterpret
        (\case
            GetTime -> id <$> input)

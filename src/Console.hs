{-# LANGUAGE TemplateHaskell #-}

module Console where

import Prelude
import Polysemy
import Polysemy.Input
import Polysemy.Writer
import qualified System.IO as SysIO

import Data.Text (Text, pack, unpack)

data Console m a where
    GetLine :: Console m Text
    PutTextLn :: Text -> Console m ()

makeSem ''Console

interpretConsoleIO :: Member (Final IO) r => Sem (Console ': r) a -> Sem r a
interpretConsoleIO =
    interpret 
        (\case
            GetLine -> embedFinal $ pack <$> SysIO.getLine
            PutTextLn msg -> embedFinal $ putStrLn (unpack msg))

interpretConsolePure :: Members '[Writer [Text]] r => [Text] -> Sem (Console ': r) a -> Sem r a
interpretConsolePure i =
    runInputList i . reinterpret
        (\case
            PutTextLn msg -> tell [msg]
            GetLine -> maybe "" id <$> input)

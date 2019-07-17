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

interpretConsoleIO :: Member (Lift IO) r => Sem (Console ': r) a -> Sem r a
interpretConsoleIO =
    interpret 
        (\case
            GetLine -> sendM $ pack <$> SysIO.getLine
            PutTextLn msg -> sendM $ putStrLn (unpack msg))

interpretConsolePure :: Members '[Writer [Text]] r => [Text] -> Sem (Console ': r) a -> Sem r a
interpretConsolePure i =
    runListInput i . reinterpret
        (\case
            PutTextLn msg -> tell [msg]
            GetLine -> maybe "" id <$> input)

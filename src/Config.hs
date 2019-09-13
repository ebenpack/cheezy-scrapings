{-# LANGUAGE TemplateHaskell #-}

module Config where

import Prelude hiding (concat)
import Polysemy

import Database.SQLite.Simple
import Database.Beam
import Database.Beam.Sqlite

import Database

data Config m a where
    GetConfig :: Int -> Config m (Maybe ConfigEntry)
    UpdateConfig :: ConfigEntry -> Config m ()

makeSem ''Config

interpretConfigIO :: Member (Embed IO) r => Sem (Config ': r) a -> Sem r a
interpretConfigIO =
    interpret 
        (\case
            GetConfig id_ -> embed $ do
                conn <- open "./cheezy-scrapings.db"
                result <- runBeamSqlite conn $ runSelectReturningOne $ select $ do
                    entry <- all_ (_config cheezyScrapingsDb)
                    guard_ (_configId entry ==. val_ id_)
                    pure entry
                close conn
                pure result
            UpdateConfig config -> embed $ do
                conn <- open "./cheezy-scrapings.db"
                runBeamSqlite conn $ do
                    Just _ <- runSelectReturningOne $ lookup_ (_config cheezyScrapingsDb) (ConfigEntryId $ _configId config)
                    runUpdate $ save (_config cheezyScrapingsDb) $ config
                close conn)

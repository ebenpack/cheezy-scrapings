{-# LANGUAGE TemplateHaskell #-}

module Blacklist where

import Prelude hiding (concat)
import Polysemy

import Database.SQLite.Simple
import Database.Beam
import Database.Beam.Sqlite

import Database

import Data.Text (Text)

data Blacklist m a where
    GetBlacklist :: Text -> Blacklist m (Maybe BlacklistEntry)
    InsertBlacklist :: Text -> Blacklist m ()

makeSem ''Blacklist

interpretBlacklistIO :: Member (Embed IO) r => Sem (Blacklist ': r) a -> Sem r a
interpretBlacklistIO =
    interpret 
        (\case
            GetBlacklist txt -> embed $ do
                conn <- open "./cheezy-scrapings.db"
                runBeamSqlite conn $ runSelectReturningOne $ select $ do
                    entry <- all_ (_blacklist cheezyScrapingsDb)
                    guard_ (_blacklistEntry entry ==. val_ txt)
                    pure entry
            InsertBlacklist txt -> embed $ do
                conn <- open "./cheezy-scrapings.db"
                runBeamSqlite conn $ runInsert $ insert (_blacklist cheezyScrapingsDb) $
                    insertExpressions [ BlacklistEntry (val_ txt) default_ ]
                pure ())

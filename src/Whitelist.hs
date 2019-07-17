{-# LANGUAGE TemplateHaskell #-}

module Whitelist where

import Prelude hiding (concat)
import Polysemy

import Database.SQLite.Simple
import Database.Beam
import Database.Beam.Sqlite

import Database

import Data.Text (Text)

data Whitelist m a where
    GetWhitelist :: Text -> Whitelist m (Maybe WhitelistEntry)
    InsertWhitelist :: Text -> Whitelist m ()

makeSem ''Whitelist

interpretWhitelistIO :: Member (Lift IO) r => Sem (Whitelist ': r) a -> Sem r a
interpretWhitelistIO =
    interpret 
        (\case
            GetWhitelist txt -> sendM $ do
                conn <- open "./cheezy-scrapings.db"
                runBeamSqlite conn $ runSelectReturningOne $ select $ do
                    entry <- all_ (_whitelist cheezyScrapingsDb)
                    guard_ (_whitelistEntry entry ==. val_ txt)
                    pure entry
            InsertWhitelist txt -> sendM $ do
                conn <- open "./cheezy-scrapings.db"
                runBeamSqlite conn $ runInsert $ insert (_whitelist cheezyScrapingsDb) $
                    insertExpressions [ WhitelistEntry (val_ txt) default_ ]
                pure ())

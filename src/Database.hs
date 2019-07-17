{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

module Database where

import Database.Beam

import Data.Time.Clock
import Data.Text (Text)

---------------------
----- Blacklist -----
---------------------

data BlacklistEntryT f
    = BlacklistEntry
    { _blacklistEntry :: Columnar f Text
    , _blacklistId :: Columnar f Int
    }
    deriving Generic

type BlacklistEntry = BlacklistEntryT Identity
type BlacklistEntryId = PrimaryKey BlacklistEntryT Identity

deriving instance Show BlacklistEntry
deriving instance Eq BlacklistEntry
instance Beamable BlacklistEntryT

instance Table BlacklistEntryT where
    data PrimaryKey BlacklistEntryT f = BlacklistEntryId (Columnar f Int) deriving (Generic, Beamable)
    primaryKey = BlacklistEntryId . _blacklistId

---------------------
----- Whitelist -----
---------------------

data WhitelistEntryT f
    = WhitelistEntry
    { _whitelistEntry :: Columnar f Text
    , _whitelistId :: Columnar f Int
    }
    deriving Generic

type WhitelistEntry = WhitelistEntryT Identity
type WhitelistEntryId = PrimaryKey WhitelistEntryT Identity

deriving instance Show WhitelistEntry
deriving instance Eq WhitelistEntry
instance Beamable WhitelistEntryT

instance Table WhitelistEntryT where
    data PrimaryKey WhitelistEntryT f = WhitelistEntryId (Columnar f Int) deriving (Generic, Beamable)
    primaryKey = WhitelistEntryId . _whitelistId

---------------------
------ Config -------
---------------------

data ConfigEntryT f
    = ConfigEntry
    { _configId :: Columnar f Int
    , _configLastIndexedLink :: Columnar f Text
    , _configLastIndexedTime :: Columnar f UTCTime
    }
    deriving Generic

type ConfigEntry = ConfigEntryT Identity
type ConfigEntryId = PrimaryKey ConfigEntryT Identity

deriving instance Show ConfigEntry
deriving instance Eq ConfigEntry
instance Beamable ConfigEntryT

instance Table ConfigEntryT where
    data PrimaryKey ConfigEntryT f = ConfigEntryId (Columnar f Int) deriving (Generic, Beamable)
    primaryKey = ConfigEntryId . _configId

---------------------
----- Database ------
---------------------

data CheezyScrapingsDb f = CheezyScrapingsDb
    { _blacklist :: f (TableEntity BlacklistEntryT)
    , _whitelist :: f (TableEntity WhitelistEntryT)
    , _config :: f (TableEntity ConfigEntryT)
    }
    deriving (Generic, Database be)

cheezyScrapingsDb :: DatabaseSettings be CheezyScrapingsDb
cheezyScrapingsDb = defaultDbSettings

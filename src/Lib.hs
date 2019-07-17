module Lib (runCheezyScrapingsIO) where

import Prelude hiding (take, concat, getLine, max)
import Polysemy
import Polysemy.State
import Polysemy.Error

import Control.Monad
import Control.Monad.Loops
import Data.Maybe (isNothing)
import Data.Set (Set, insert, empty)
import Data.Text (Text, pack, append, concat, toLower)
import Data.Time.Clock

import Database
import Blacklist
import Config
import Console
import FetchLinks
import Time
import Whitelist

data ProgramConfig = ProgramConfig
    { configEntry :: Maybe ConfigEntry
    , currentPage :: Int
    , hits :: Set Link
    , maybeHits :: Set Link
    , done :: Maybe Int
    , maxPage :: Int
    , extraPages :: Int
    , lastIndexed :: Maybe Text
    }

fetchLinks :: Members '[Error NetworkError, Console, FetchLinks] r => String -> Sem r [Link]
fetchLinks path = catch (getLinks path) cleanup
    where
    cleanup :: Members '[Console] r => NetworkError -> Sem r [Link]
    cleanup _ = do
        putTextLn "ERROR!"
        pure []

scrapeItCheezy :: Members '[Error NetworkError, Console, FetchLinks, Blacklist, Whitelist, Config, Time, State ProgramConfig] r => Sem r ()
scrapeItCheezy = do
    dbConfig <- getConfig 1 -- TODO: This is dumb
    modify (\s -> s { configEntry = dbConfig})
    printLastRunTime
    _ <- processPage `untilM` doneProcessing
    printHits
    processMaybeHits
    updateLastRunTime
    where
    url :: Int -> String
    url pageNum = "https://eztv.ag/" ++ if pageNum == 0 then "" else "page_" ++ show pageNum
    getConfigBy :: Members '[State ProgramConfig] r => (ProgramConfig -> a) -> Sem r a
    getConfigBy getter = get >>= pure . getter
    processPage :: Members '[Error NetworkError, Console, FetchLinks, Blacklist, Whitelist, Config, State ProgramConfig] r => Sem r ()
    processPage = do
        page <- getConfigBy currentPage
        putTextLn $ append "Fetching page: " (pack $ show page)
        links <- fetchLinks $ url page
        lastIndexedLink <- getConfigBy (\config -> maybe "" (\(ConfigEntry _ t _) -> t) (configEntry config))
        forM_ links (processLink lastIndexedLink page)
        modify (\s ->
            let end = maxPage s
            in s 
                { currentPage = (currentPage s) + 1
                , done = case done s of
                    Just d' -> Just d'
                    Nothing -> if page > end then Just page else Nothing
                })
    processLink :: Members '[Console, Blacklist, Whitelist, State ProgramConfig] r => Text -> Int -> Link -> Sem r ()
    processLink previousltLastIndexed page link = do
        when (href link == previousltLastIndexed) (modify (\s -> s { done = Just page }))
        config <- get
        when (isNothing $ lastIndexed config) (modify (\s -> s { lastIndexed = Just $ href link }))
        bl <- getBlacklist $ showName link
        case bl of
            Just _ -> pure ()
            Nothing -> do
                wl <- getWhitelist $ showName link
                if null wl
                then
                    modify (\s -> s { maybeHits = insert link (maybeHits s) })
                else
                    modify (\s -> s { hits = insert link (hits s) })
    doneProcessing :: Members '[State ProgramConfig] r => Sem r Bool
    doneProcessing = do
        config <- get
        let page = currentPage config
            completed = done config
            extras = extraPages config
            max = maxPage config
        case completed of
            Nothing -> pure $ page > max
            Just end -> pure $ page > (end + extras) || page > max
    printHits :: Members '[Console, State ProgramConfig] r => Sem r ()
    printHits = do
        config <- get
        putTextLn "---------------------"
        putTextLn "Hits:"
        let allHits = uniqueHits (hits config)
        forM_ allHits putTextLn
        putTextLn "---------------------"
    processMaybeHits :: Members '[Console, Config, Blacklist, Whitelist, Time, State ProgramConfig] r => Sem r ()
    processMaybeHits = do
        uniqueMaybes <- getConfigBy $ uniqueHits . maybeHits
        forM_ uniqueMaybes hitDecider
        configEntry' <- getConfigBy configEntry
        lastIndexed' <- getConfigBy lastIndexed
        case (configEntry', lastIndexed') of
            (Just ce, Just li) -> updateConfig (ce { _configLastIndexedLink = li })
            _ -> pure ()
        where
        hitDecider :: Members '[Console, Blacklist, Whitelist, State ProgramConfig] r => Text -> Sem r ()
        hitDecider name = do
            putTextLn $ concat ["Keep \"", name, "\"? (y/N/i(gnore))"]
            answer <- getLine
            case toLower answer of
                "y" -> insertWhitelist name
                "n" -> insertBlacklist name
                "i" -> pure ()
                "" -> insertBlacklist name
                _ -> putTextLn "Input not recognized" >> hitDecider name
    printLastRunTime :: Members '[Console, Time, State ProgramConfig] r => Sem r ()
    printLastRunTime = do
        config <- get
        t <- getTime    
        case configEntry config of
            Nothing -> pure ()
            Just c -> putTextLn $ concat
                [ "Last run: "
                , pack $ show $ _configLastIndexedTime c
                , "; "
                , pack $ formatTimeDiff t (_configLastIndexedTime c)
                , " ago"
                ]
        where
        formatTimeDiff start end = go seconds
            where
            seconds :: Integer
            seconds = round $ toRational $ diffUTCTime start end
            go :: Integer -> String
            go secs
                | secs > 86400 = (show (secs `div` 86400) ++ "d") ++ (go (secs `mod` 86400))
                | secs > 3600 = (show (secs `div` 3600) ++ "h") ++ (go (secs `mod` 3600))
                | secs > 60 = (show (secs `div` 60) ++ "m") ++ (go (secs `mod` 60))
                | otherwise = show secs ++ "s"
    updateLastRunTime :: Members '[Time, Config, State ProgramConfig] r => Sem r ()
    updateLastRunTime = do
        t <- getTime
        configEntry' <- getConfigBy configEntry
        case configEntry' of
            Just ce -> updateConfig (ce { _configLastIndexedTime = t })
            _ -> pure ()
    uniqueHits :: Set Link -> Set Text
    uniqueHits = foldr (\link links -> insert (showName link) links) empty


runCheezyScrapingsIO :: IO (Either NetworkError (ProgramConfig, ()))
runCheezyScrapingsIO
    = (runM .@@ runErrorInIO @NetworkError)
    . (runState initialState)
    . interpretConsoleIO
    . interpretRequestIO
    . interpretBlacklistIO
    . interpretWhitelistIO
    . interpretConfigIO
    . interpretTimeIO
    $ scrapeItCheezy
    where
    initialState :: ProgramConfig
    initialState = ProgramConfig
        { configEntry = Nothing
        , currentPage = 0
        , done = Nothing
        , hits = empty
        , maybeHits = empty
        , maxPage = 100
        , extraPages = 5
        , lastIndexed = Nothing
        }

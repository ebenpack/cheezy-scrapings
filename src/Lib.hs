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
    processMaybeHits
    printHits
    saveConfig
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
    processLink previouslyLastIndexed page link = do
        when (href link == previouslyLastIndexed) (modify (\s -> s { done = Just page }))
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
        putTextLn "---------------------"
        putTextLn "Hits:"
        config <- get
        let allHits = uniqueHits (hits config)
        forM_ allHits hitPrinter
        where
        hitPrinter :: Members '[Console] r => (Text, Maybe Text) -> Sem r ()
        hitPrinter (name, ep) = putTextLn $ concat [name, maybe "" (append " - ") ep]
    processMaybeHits :: Members '[Console, Config, Blacklist, Whitelist, Time, State ProgramConfig] r => Sem r ()
    processMaybeHits = do
        putTextLn "---------------------"
        uniqueMaybes <- getConfigBy $ uniqueHits . maybeHits
        forM_ (foldr (\a b -> insert (fst a) b) empty uniqueMaybes) hitDecider
        configEntry' <- getConfigBy configEntry
        lastIndexed' <- getConfigBy lastIndexed
        case (configEntry', lastIndexed') of
            (Just ce, Just li) ->
                updateConfig (ce { _configLastIndexedLink = li })
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
    saveConfig :: Members '[Time, Config, State ProgramConfig] r => Sem r ()
    saveConfig = do
        t <- getTime
        configEntry' <- getConfigBy configEntry
        lastIndexedUrl <- getConfigBy lastIndexed
        case (configEntry', lastIndexedUrl) of
            (Just ce, Just li) -> updateConfig (ce { _configLastIndexedTime = t, _configLastIndexedLink = li })
            _ -> pure ()
    uniqueHits :: Set Link -> Set (Text, Maybe Text)
    uniqueHits = foldr (\link links ->
        let episodeNumber = episode link
            episodeName = showName link
        in insert (episodeName, episodeNumber) links) empty

runCheezyScrapingsIO :: IO (Either NetworkError (ProgramConfig, ()))
runCheezyScrapingsIO
    = runFinal
    . embedToFinal @IO
    . errorToIOFinal @NetworkError
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

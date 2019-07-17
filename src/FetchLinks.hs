{-# LANGUAGE TemplateHaskell #-}

module FetchLinks where

import Polysemy
import Polysemy.Error hiding (catch)

import Control.Applicative
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.Text (Text, pack)
import Text.HTML.Scalpel

data NetworkError = NetworkError deriving (Show)

data FetchLinks m r where
    GetLinks :: String -> FetchLinks m [Link]

data Link = Link 
    { showName :: Text
    , episode :: Maybe Text, href :: Text 
    } deriving (Show, Eq, Ord)

parseLink :: Text -> Text -> Link
parseLink link raw =
    case parsed raw of
        Left f -> Link raw Nothing (pack f)
        Right l -> l
    where
    parsed =
        parseOnly $ do
            name <- manyTill anyChar (lookAhead (space *> parseEpisode))
            ep <- space *> parseEpisode <* skipMany anyChar <* endOfInput
            pure $ Link (pack name) (Just $ pack ep) link
            
    parseEpisode = parseSeasonAndEpisode <|> parseYear
    parseYear = do 
        _ <- string "20"
        d <- count 2 digit
        pure $ "20" ++ d
    parseSeasonAndEpisode = do
        s <- char 'S' <|> char 's'
        sn <- many1 digit
        e <- char 'E' <|> char 'e'
        en <- many1 digit
        pure $ (s : sn) ++ (e : en)

allComments :: String -> IO (Either NetworkError [Link])
allComments path = do
    results <- scrapeURL path links
    case results of
        Nothing -> pure $ Left NetworkError
        Just r -> pure $ Right r
    where
        links :: Scraper String [Link]
        links = chroots ("tr" @: [hasClass "forum_header_border"]) $ do
            ep <- text ("a" @: [hasClass "epinfo"])
            link <- attr "href" ("a" @: [hasClass "magnet"])
            pure $ parseLink (pack link) (pack ep)

makeSem ''FetchLinks

interpretRequestIO :: Members '[Lift IO, Error NetworkError] r
  => Sem (FetchLinks ': r) a
  -> Sem r a
interpretRequestIO =
    interpret
        (\case
            GetLinks path -> fromEitherM $ allComments path)

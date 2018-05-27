{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Database where

import Scraper

import Control.Monad ((>=>))
import Data.Foldable (traverse_)
import Data.Monoid ((<>))

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

instance FromRow Card where
  fromRow = Card <$> field <*> field

instance ToRow Card where
  toRow (Card name gathererURL) = toRow (name, gathererURL)  

makeGathererTable :: String -> IO ()
makeGathererTable dbPath = do
  conn <- open dbPath
  execute_ conn "CREATE TABLE IF NOT EXISTS gatherer (name TEXT, url TEXT, CONSTRAINT name_unique UNIQUE(name));"
  insertAllCards conn
  close conn

insertCardsFromSet :: Connection -> [Card] -> IO ()
insertCardsFromSet _    []             = pure ()
insertCardsFromSet conn (card : cards) =
  let escapeQuotes text = T.replace "\"" "\"\"" $ T.replace "'" "''" text
      values_pair (Card name url) = "(\"" <> escapeQuotes name <> "\",\"" <> url <> "\")"
      query = Query $ "INSERT OR IGNORE INTO gatherer (name, url) VALUES "
                    <> values_pair card
                    <> foldr (\ c str -> ", " <> c <> str) ";" (map values_pair cards)
  in execute_ conn query          

insertAllCards :: Connection -> IO ()
insertAllCards conn = getSetNames >>= traverse_ (getCardsFromSet >=> insertCardsFromSet conn)

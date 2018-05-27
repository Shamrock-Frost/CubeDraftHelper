{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Database where

import Control.Monad (unless)
import Data.Foldable (for_, traverse_)
import Data.Maybe (maybe, fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Text.Read (readMaybe)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.URI.Encode (encode)

import Text.HTML.Scalpel (Scraper, Selector, (@=), (@:), scrapeURL,
                          chroot, chroots, text, texts, attr, 
                          anySelector, tagSelector, hasClass)

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data Card = Card { name :: T.Text, gathererURL :: T.Text }
  deriving Show

instance FromRow Card where
  fromRow = Card <$> field <*> field

instance ToRow Card where
  toRow (Card name gathererURL) = toRow (name, gathererURL)  

mkDatabase :: IO ()
mkDatabase = do
  conn <- open "/home/brendan/Dev/Haskell/CubeDraftHelper/server/mtg.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS gatherer (name TEXT, url TEXT, CONSTRAINT name_unique UNIQUE(name));"
  insertAllCards conn
  close conn

gathererHomePage :: String
gathererHomePage = "http://gatherer.wizards.com/Pages/Default.aspx"

searchSetURL :: String -> Maybe Int -> String
searchSetURL setName pageNumber =
  concat [ "http://gatherer.wizards.com/Pages/Search/Default.aspx?"
         , "set=[\""
         , encode setName
         , "\"]"
         ] ++ maybe "" (\ n -> "&page=" ++ show n) pageNumber

insertAllCards :: Connection -> IO ()
insertAllCards conn = getSetNames >>= traverse_ (insertCardsFromSet conn) . map T.unpack

insertCardsFromSet :: Connection -> String -> IO ()
insertCardsFromSet conn setName = do
  Just pages <- scrapeURL (searchSetURL setName Nothing) pageCount
  let pageURLs = [searchSetURL setName (Just n) | n <- [0..pages - 1]]
  for_ pageURLs $ \ pageURL -> do
    Just batch <- scrapeURL pageURL cards
    unless (null batch) $ execute_ conn (buildInsertQuery batch)
  where relativeCardURLToAbsolute = T.append "http://gatherer.wizards.com/Pages/" . T.drop 3
        pageCount :: Scraper String Int
        pageCount = fmap (lastOrDefault 1 . mapMaybe readMaybe)
                  $ chroot ("div" @: ["id" @= "ctl00_ctl00_ctl00_MainContent_SubContent_topPagingControlsContainer"])
                  $ texts $ tagSelector "a"
        cards :: Scraper T.Text [Card]
        cards = chroots ("span" @: [hasClass "cardTitle"])
              $ Card <$> fmap T.stripStart (text anySelector)
                     <*> fmap relativeCardURLToAbsolute
                              (attr "href" $ tagSelector "a")
        buildInsertQuery :: [Card] -> Query
        buildInsertQuery [] = ""
        buildInsertQuery (card : cards) = Query $
          let preamble = "INSERT OR IGNORE INTO gatherer (name, url) VALUES "
              escapeQuotes text = T.replace "\"" "\"\"" $ T.replace "'" "''" text
              values_pair (Card name url) = "(\"" <> escapeQuotes name <> "\",\"" <> url <> "\")"
          in preamble <> values_pair card <> foldr (\ c str -> ", " <> c <> str) ";" (map values_pair cards)

getSetNames :: IO [T.Text]
getSetNames = fromMaybe [] <$> scrapeURL gathererHomePage setNames
  where setNames :: Scraper T.Text [T.Text]
        setNames = chroot setList $ filter (not . T.null) <$> texts (tagSelector "option")
        
        setList :: Selector
        setList = "select" @: ["id" @= "ctl00_ctl00_MainContent_Content_SearchControls_setAddText"]

lastOrDefault :: a -> [a] -> a
lastOrDefault a []     = a
lastOrDefault _ [x]    = x
lastOrDefault a (x:xs) = lastOrDefault a xs

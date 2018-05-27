{-# LANGUAGE OverloadedStrings #-}

module Scraper where

import Data.Maybe (maybe, fromMaybe, mapMaybe)
import Control.Monad (unless)
import Data.Monoid ((<>))
import Data.Traversable (for)
import Text.Read (readMaybe)

import qualified Data.Text as T

import Text.HTML.Scalpel (Scraper, Selector, (@=), (@:), scrapeURL,
                          chroot, chroots, text, texts, attr, 
                          anySelector, tagSelector, hasClass)
import Network.URI.Encode (encode)

data Card = Card { name :: T.Text, gathererURL :: T.Text }
  deriving Show

gathererRootURL :: String
gathererRootURL = "http://gatherer.wizards.com/Pages/"

setPageURL :: String -> String
setPageURL setName = gathererRootURL
                     ++ "Search/Default.aspx?"
                     ++ "set=[\""
                     ++ encode setName
                     ++ "\"]"

getSetNames :: IO [String]
getSetNames = fromMaybe [] <$> scrapeURL (gathererRootURL ++ "Default.aspx") setNames
  where setNames :: Scraper String [String]
        setNames = chroot setList $ filter (not . null) <$> texts (tagSelector "option")

        setList :: Selector
        setList = "select" @: ["id" @= "ctl00_ctl00_MainContent_Content_SearchControls_setAddText"]

getCardsFromSet :: String -> IO [Card]
getCardsFromSet setName = concat <$> do
  Just pageCount <- scrapeURL (setPageURL setName) pageCountScraper
  let pageURLs = [setPageURL setName ++ "&page=" ++ show n | n <- [0..pageCount - 1]]
  for pageURLs $ \pageURL -> fromMaybe [] <$> scrapeURL pageURL cardsScraper
  where pageCountScraper :: Scraper String Int
        pageCountScraper = 
          let linksID = "ctl00_ctl00_ctl00_MainContent_SubContent_topPagingControlsContainer"
          in fmap (lastOrDefault 1 . mapMaybe readMaybe)
             $ chroot ("div" @: ["id" @= linksID])
             $ texts $ tagSelector "a"
        cardsScraper :: Scraper T.Text [Card]
        cardsScraper =
          let makeURLAbsolute = T.append (T.pack gathererRootURL) . T.drop 3
          in chroots ("span" @: [hasClass "cardTitle"])
             $ Card <$> fmap T.stripStart (text anySelector)
                    <*> fmap makeURLAbsolute (attr "href" $ tagSelector "a")

lastOrDefault :: a -> [a] -> a
lastOrDefault a []     = a
lastOrDefault _ [x]    = x
lastOrDefault a (x:xs) = lastOrDefault a xs

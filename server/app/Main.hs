{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Main where

import Database

import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import Data.IORef
import qualified Data.Text as T
import qualified Data.Set as S

data PlayerStatus = Host | Join deriving (Show, Read)

data Session = EmptySession
data AppState = NoAction
              | InLobby PlayerStatus

main :: IO ()
main = mkDatabase {-
       do
  ref <- newIORef NoAction
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase ref
  runSpock 8000 (spock spockCfg app)
-}

app :: SpockM () Session (IORef AppState) ()
app = do get (root <//> static "host") $ handleReq Host
         get (root <//> static "join") $ handleReq Join
  where handleReq :: PlayerStatus -> ActionCtxT () (WebStateM () Session (IORef AppState)) ()
        handleReq x = do
          ref <- getState
          state <- liftIO $ readIORef ref
          case state of
            NoAction  -> do liftIO $ writeIORef ref (InLobby x)
                            setHeader "Access-Control-Allow-Origin" "*"
                            text (T.pack $ "Begin process \"" ++ show x ++ "\"")
            InLobby y -> do liftIO $ writeIORef ref (InLobby x)
                            setHeader "Access-Control-Allow-Origin" "*"
                            text (T.pack $ "Change from process \""
                                         ++ show y ++ "\" to process \""
                                         ++ show x ++ "\"")

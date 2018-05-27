{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Main where

import Database

import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import Data.IORef
import qualified Data.Text as T

import Database.SQLite.Simple (open, close, Connection)

data PlayerStatus = Host | Join deriving (Show, Read)

data Session = EmptySession
data AppState = NoAction
              | InLobby PlayerStatus

testDatabasePath :: String
testDatabasePath = "/home/brendan/Dev/Haskell/CubeDraftHelper/server/mtg.db"

main :: IO ()
main = do
  ref <- newIORef NoAction
  let sqlConnBuilder = PCConn $ ConnBuilder
                              { cb_createConn=open testDatabasePath
                              , cb_destroyConn=close
                              , cb_poolConfiguration=PoolCfg 1 1 30
                              }
  spockCfg <- defaultSpockCfg EmptySession sqlConnBuilder ref
  runSpock 8000 (spock spockCfg app)

app :: SpockM Connection Session (IORef AppState) ()
app = do get (root <//> static "host") $ handleReq Host
         get (root <//> static "join") $ handleReq Join
  where handleReq :: PlayerStatus -> ActionCtxT () (WebStateM Connection Session (IORef AppState)) ()
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

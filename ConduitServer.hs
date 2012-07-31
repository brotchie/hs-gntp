{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Conduit
import Data.Conduit.Network

import Network.GNTP
import Data.Conduit.GNTP (gntpConduitM)

main :: IO ()
main = runTCPServer (ServerSettings 9090 "*") application

application :: Application IO
application source sink = source 
                       $= gntpConduitM process
                       $$ sink

process :: Request -> IO Response
process r = do 
  let reqType = requestType r

  case reqType of
    Register  -> putStrLn "Register"
    Notify    -> putStrLn "Notify"
    Subscribe -> putStrLn "Subscribe"

  return $ createOkResponse reqType

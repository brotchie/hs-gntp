module Main where

import IO
import Network

import Control.Concurrent
import Control.Applicative

import Data.Attoparsec.Lazy (Result (..))
import qualified Data.ByteString.Lazy.Char8 as LBS

import Network.GNTP

serverPort :: PortID
serverPort = PortNumber 9090

main :: IO ()
main = withSocketsDo gntpServer

gntpServer :: IO ()
gntpServer = listenOn serverPort >>= eventLoop

eventLoop :: Socket -> IO ()
eventLoop socket = accept socket >>= forkIO . handleClient >>
                   eventLoop socket

handleClient :: (Handle, HostName, PortNumber) -> IO ()
handleClient (h, _, _) = do 
  request <- parseRequest <$> LBS.hGetContents h
  LBS.hPut h $ encodeResponse $ case request of
    (Fail _ _ _) -> createErrorResponse 200
    (Done _ req) -> handleRequest req
  hClose h

handleRequest :: Request -> Response
handleRequest request = createOkResponse $ requestType request

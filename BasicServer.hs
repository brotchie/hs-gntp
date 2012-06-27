module Main where

import IO (Handle, hPutStrLn, hClose)
import Network (PortID(..), Socket, withSocketsDo, listenOn, accept)

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Attoparsec.Lazy (Result (..))

import Network.GNTP

serverPort :: PortID
serverPort = PortNumber 9090

main :: IO ()
main = withSocketsDo $ do socket <- listenOn serverPort
                          eventLoop socket

eventLoop :: Socket -> IO ()
eventLoop socket = do
    (h, _, _) <- accept socket
    contents <- LBS.hGetContents h
    let result = parseRequest contents
    case result of
        (Fail _ context error) -> putStrLn $ (show context) ++ " : " ++ error
        (Done _ request) -> handleRequest request h
    hClose h
    eventLoop socket

gntpVersion :: Version
gntpVersion = Version 1 0

handleRequest :: Request -> Handle -> IO()
handleRequest request h = do
    let response = Response gntpVersion (Ok $ requestType request) Nothing
    print $ request
    LBS.hPut h $ encodeResponse response

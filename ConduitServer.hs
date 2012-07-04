{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString as BS hiding (putStrLn)
import qualified Data.ByteString.Lazy as LBS

import Data.Attoparsec.Char8

import Data.Conduit
import Data.Conduit.Network
import qualified Data.Conduit.List as CL

import Network.GNTP

main :: IO ()
main = runTCPServer (ServerSettings 9090 "*") application

application :: Application IO
application source sink = source 
                       $= parseMessage'
                       $= processMessage'
                       $= encodeResponse'
                       $$ sink

-- Wrap request parsing, processing, and response encoding
-- in conduits.

-- | Takes a stream of ByteString chunks and produces
-- Maybe GNTP.Request instances.
parseMessage' :: Conduit ByteString IO (Maybe Request)
parseMessage' = CL.concatMapAccumM parseMessage Nothing

-- | Takes a series of Maybe GNTP.Request instances and
-- produces GNTP.Response instances.
processMessage' :: Conduit (Maybe Request) IO Response
processMessage' = CL.mapM processMessage

-- | Takes a series of GNTP.Response instances and encodes
-- them into ByteStrings.
encodeResponse' :: Conduit Response IO ByteString
encodeResponse' = CL.concatMap (LBS.toChunks . encodeResponse)

processMessage :: Maybe Request -> IO Response
processMessage (Just request) = do print reqType
                                   return $ createOkResponse reqType
                                where reqType = requestType request
processMessage Nothing        = return $ createErrorResponse 200

-- We feed incoming ByteStrings into the request parser, carrying along
-- the parsing state.
parseMessage :: ByteString -> Maybe (Result Request) -> IO (Maybe (Result Request), [Maybe Request])
parseMessage bs state = do let result' = case state of
                                   Just result -> feed result bs
                                   Nothing     -> parse requestParser bs
                           case result' of
                             Done _ request -> return (Nothing, [Just request])
                             Fail _ _ _     -> return (Nothing, [Nothing])
                             _              -> return (Just result', [])

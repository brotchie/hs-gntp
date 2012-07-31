module Data.Conduit.GNTP 
  (
  -- * High level conduits.
    gntpConduit
  , gntpConduitM

  -- * Lower level components.
  , processRequest
  , processRequestM
  , parseRequest'
  , encodeResponse'
  ) where


import Data.ByteString.Char8
import qualified Data.ByteString.Lazy as LBS

import Data.Conduit
import qualified Data.Conduit.List as CL

import Data.Attoparsec.Char8

import Network.GNTP hiding (parseRequest)

gntpConduit :: (Monad m) => (Request -> Response) -> Conduit ByteString m ByteString
gntpConduit f = parseRequest' =$= processRequest f =$= encodeResponse'

gntpConduitM :: (Monad m) => (Request -> m Response) -> Conduit ByteString m ByteString
gntpConduitM f = parseRequest' =$= processRequestM f =$= encodeResponse'

processRequest :: (Monad m) => (Request -> Response) -> Conduit (Maybe Request) m Response
processRequest f = CL.map handleRequest
               where handleRequest (Just request) = f request
                     handleRequest Nothing        = createErrorResponse 200

processRequestM :: (Monad m) => (Request -> m Response) -> Conduit (Maybe Request) m Response
processRequestM f = CL.mapM handleRequest
                where handleRequest (Just request) = f request
                      handleRequest Nothing        = return $ createErrorResponse 200

encodeResponse' :: (Monad m) => Conduit Response m ByteString
encodeResponse' = CL.concatMap (LBS.toChunks . encodeResponse)

parseRequest' :: (Monad m) => Conduit ByteString m (Maybe Request)
parseRequest' = CL.concatMapAccumM parseRequest Nothing

parseRequest :: (Monad m) => ByteString -> Maybe (Result Request) -> m (Maybe (Result Request), [Maybe Request])
parseRequest bs state = do
  let result' = case state of
                  Just result -> feed result bs
                  Nothing     -> parse requestParser bs
  case result' of
    Done _ request -> return (Nothing, [Just request])
    Fail _ _ _     -> return (Nothing, [Nothing])
    _              -> return (Just result', [])

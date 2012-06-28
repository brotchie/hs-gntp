{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
-- |
-- Module:      Network.GNTP
-- Copyright:   (c) 2012 James Brotchie
-- Maintainer:  James Brotchie <brotchie@gmail.com>
-- Stability:   experimental
--
-- This module implements request parsing and response encoding for
-- Version 1.0 of the Growl Notification Transport Protocol (GNTP).
-- It was developed to proxy Growl notifications to other services.
--
-- A full description of Version 1.0 of the GNTP protocol
-- is available at <http://www.growlforwindows.com/gfw/help/gntp.aspx>.
--
module Network.GNTP 
    (
    -- * Core GNTP types
      Request (..)
    , Version (..)
    , RequestType (..)
    , EncryptionAlgorithm
    , Response (..)
    , ResponseType (..)
    , ErrorCode, ErrorDescription
    -- * Header types
    , Header (..)
    , StringValue, IntValue, BoolValue
    , UniqueID (..), URL (..)
    , ConnectionDirective (..)
    , Headers, NotificationHeaders
    -- * Request helpers
    , requestType
    -- * Request parsing
    , parseRequest
    -- * Response encoding
    , encodeResponse
    , createOkResponse
    , createErrorResponse
    , createErrorResponseWithDescription
    , defaultVersion
    ) where

import Control.Monad (when)
import Data.Maybe (fromJust, isJust)

import Control.Applicative ((<*),(<*>),(<$>),(<|>))

import Data.Binary.Put (Put, runPut, putByteString)
import Data.ByteString.Char8 hiding (map, count)
import Data.Attoparsec.Char8 hiding (parse, Result)
import Data.Attoparsec.Lazy (parse, Result)
import qualified Data.ByteString.Lazy.Char8 as LBS

-- | A full description of a GNTP request. 
--
-- Use 'parseRequest' returns to build a 'Request' instance
-- from a lazy 'LBS.ByteString'.
data Request = Request Version RequestType (Maybe EncryptionAlgorithm) Headers [NotificationHeaders] deriving Show

data RequestType = Register
                 | Notify
                 | Subscribe
                 deriving (Show, Eq)

-- | GNTP protocol version. 1.0 is the only known version, and is retrievable
-- using 'defaultVersion'.
data Version = Version Int Int 
             deriving (Show, Eq)

-- | The default GNTP version.
defaultVersion :: Version
defaultVersion = Version 1 0


data Response = Response Version ResponseType (Maybe EncryptionAlgorithm)
              deriving Show
data ResponseType = Ok RequestType
                  | Error ErrorCode (Maybe ErrorDescription)
                  deriving (Show, Eq)

type ErrorCode = Int
type ErrorDescription = ByteString

-- | Not implemented as yet.
data EncryptionAlgorithm = EncryptionAlgorithm 
                         deriving Show

-- Header value types.
type    StringValue         = ByteString
type    IntValue            = Int
type    BoolValue           = Bool
-- | A unique identifier. The stored value does not contain the @\/\/@ prefix.
newtype UniqueID            = UniqueID ByteString deriving (Show, Eq)
newtype URL                 = URL ByteString deriving (Show, Eq)
data    ConnectionDirective = Close
                            | KeepAlive
                            deriving (Show, Eq)

-- | The headers defined in the GNTP 1.0 standard. Custom
-- headers are parsed as 'UnknownHeader' instances.
data Header = ApplicationName StringValue
            | ApplicationIcon (Either UniqueID URL)

            | OriginPlatformVersion StringValue
            | OriginSoftwareVersion StringValue
            | OriginMachineName StringValue
            | OriginSoftwareName StringValue
            | OriginPlatformName StringValue

            | NotificationsCount IntValue
            | NotificationName StringValue
            | NotificationDisplayName StringValue
            | NotificationEnabled BoolValue
            | NotificationIcon (Either UniqueID URL)
            | NotificationText StringValue
            | NotificationTitle StringValue

            | Connection ConnectionDirective

            -- Response headers, not used at the moment.
--            | ResponseAction RequestType
--            | ErrorCode IntValue
--            | ErrorDescription StringValue

            | UnknownHeader ByteString StringValue
            deriving Show

type Headers = [Header]
type NotificationHeaders = [Header]

-- | Extracts the 'RequestType' from a 'Request' instance.
requestType :: Request -> RequestType
requestType (Request _ reqtype _ _ _) = reqtype

-- | Encodes a 'Response' instance into a lazy 'LBS.ByteString'.
encodeResponse :: Response -> LBS.ByteString
encodeResponse (Response version resptype encrypt) = runPut $ do
    put "GNTP/"
    versionEncoder version >> putSpace
    put (responseTypeByteString resptype) >> putSpace
    encryptionAlgorithmEncoder encrypt >> putCRLF

    case resptype of
        (Ok reqtype)      -> putHeader "Response-Action" requestTypeString
                             where requestTypeString = requestTypeByteString reqtype

        (Error code desc) -> putHeader "Error-Code" code >>
                             when hasDescription putDescription
                             where hasDescription = isJust desc
                                   putDescription = putHeader "Error-Description" (fromJust desc)
    putCRLF

-- Helpers for encoding messages.
put :: ByteString -> Put
put = putByteString

putNum :: (Num a, Show a) => a -> Put
putNum x = put . pack $ show x

putSpace :: Put
putSpace = put " "

putCRLF :: Put
putCRLF = put crlf

putHeader :: (HeaderValue a) => ByteString -> a -> Put
putHeader header value = put header >> put ": " >> headerValueEncoder value >> putCRLF

crlf :: ByteString
crlf = "\r\n"

eatCRLF :: Parser ()
eatCRLF = string crlf >> return ()

takeTillDiscardLast :: Char -> Parser ByteString
takeTillDiscardLast c = takeTillChar c <* char c

takeTillChar :: Char -> Parser ByteString
takeTillChar c = takeTill (== c)

requestTypeParser :: Parser RequestType
requestTypeParser = do messageType <- takeTill isSpace
                       case messageType of
                         "REGISTER"  -> return Register
                         "NOTIFY"    -> return Notify
                         "SUBSCRIBE" -> return Subscribe
                         _           -> error $ "Invalid message type " ++ unpack messageType
                         

requestTypeByteString :: RequestType -> ByteString
requestTypeByteString Register  = "REGISTER"
requestTypeByteString Notify    = "NOTIFY"
requestTypeByteString Subscribe = "SUBSCRIBE"

responseTypeByteString :: ResponseType -> ByteString
responseTypeByteString (Ok _)    = "-OK"
responseTypeByteString (Error _ _) = "-ERROR"

encryptionAlgorithmEncoder :: Maybe EncryptionAlgorithm -> Put
encryptionAlgorithmEncoder _ = put "NONE"

encryptionAlgorithmParser :: Parser (Maybe EncryptionAlgorithm)
encryptionAlgorithmParser = string "NONE" >> return Nothing

versionEncoder :: Version -> Put
versionEncoder (Version major minor) = putNum major >>
                                       put "." >>
                                       putNum minor

versionParser :: Parser Version
versionParser = Version <$> decimal <* char '.'
                        <*> decimal

headerNameParser :: Parser ByteString
headerNameParser = takeTillDiscardLast ':' <* skipSpace

-- | A instance of 'HeaderValue a' implements a 'headerValueParser'
-- function that returns an Attoparsec parser to parse a ByteString
-- into a value of its type.
class HeaderValue a where
    headerValueParser :: Parser a
    headerValueEncoder :: a -> Put

instance HeaderValue StringValue where
    headerValueParser = takeTillChar '\r' <* eatCRLF <?> "StringValue"
    headerValueEncoder = put

instance (HeaderValue a, HeaderValue b) => HeaderValue (Either a b) where
    headerValueParser = eitherP headerValueParser headerValueParser <?> "Either"
    headerValueEncoder = either headerValueEncoder headerValueEncoder

instance HeaderValue URL where
    headerValueParser = URL <$> headerValueParser <?> "URL"
    headerValueEncoder (URL value) = put value

instance HeaderValue UniqueID where
    headerValueParser = string "//" >> UniqueID <$> headerValueParser <?> "UniqueID"
    headerValueEncoder (UniqueID value) = put "//" >> put value

instance HeaderValue IntValue where
    headerValueParser = decimal <* eatCRLF <?> "IntValue"
    headerValueEncoder = putNum

instance HeaderValue ConnectionDirective where
    headerValueParser = do value <- (string "Close" <|> string "Keep-Alive") <* eatCRLF
                           return $ if value == "Close" then Close else KeepAlive
    headerValueEncoder Close = put "Close"
    headerValueEncoder KeepAlive = put "Keep-Alive"

instance HeaderValue BoolValue where
    headerValueParser = do value <- (string "True" <|> string "False") <* eatCRLF
                           return $ value == "True"
    headerValueEncoder value = put . pack $ show value

headerBuilder :: (HeaderValue a) => (a -> Header) -> Parser Header
headerBuilder = (<$> headerValueParser)

headerParser :: Parser Header
headerParser = do
    headerName <- headerNameParser
    case headerName of
        "Connection"                -> headerBuilder Connection
        "Application-Name"          -> headerBuilder ApplicationName
        "Application-Icon"          -> headerBuilder ApplicationIcon

        "Origin-Platform-Version"   -> headerBuilder OriginPlatformVersion
        "Origin-Software-Version"   -> headerBuilder OriginSoftwareVersion
        "Origin-Machine-Name"       -> headerBuilder OriginMachineName
        "Origin-Software-Name"      -> headerBuilder OriginSoftwareName
        "Origin-Platform-Name"      -> headerBuilder OriginPlatformName

        "Notifications-Count"       -> headerBuilder NotificationsCount
        "Notification-Enabled"      -> headerBuilder NotificationEnabled
        "Notification-Name"         -> headerBuilder NotificationName
        "Notification-Display-Name" -> headerBuilder NotificationDisplayName
        "Notification-Icon"         -> headerBuilder NotificationIcon
        "Notification-Text"         -> headerBuilder NotificationText
        "Notification-Title"        -> headerBuilder NotificationTitle
        _                           -> headerBuilder $ UnknownHeader headerName

headersParser :: Parser Headers
headersParser = manyTill headerParser eatCRLF

notificationsCount :: Headers -> Int
notificationsCount (NotificationsCount x:_) = x
notificationsCount (_:xs) = notificationsCount xs
notificationsCount [] = 0

requestParser :: Parser Request
requestParser = do 
    _ <- string "GNTP/"
    version             <- versionParser             <* space               <?> "Version+Space"
    reqType             <- requestTypeParser         <* space               <?> "RequestType+Space"
    encryptionAlgorithm <- encryptionAlgorithmParser <* eatCRLF             <?> "EncryptionAlgorithm+CRLF"
    headers             <- headersParser                                    <?> "Headers"
    notifications       <- count (notificationsCount headers) headersParser <?> "Notifications"
    return $ Request version reqType encryptionAlgorithm headers notifications

-- | Attempts to parse a GNTP 'Request' from a lazy 'LBS.ByteString'.
parseRequest :: LBS.ByteString -> Result Request
parseRequest = parse requestParser


-- | Creates a response with the default GNTP version, an OK
-- response type, and no encryption.
createOkResponse :: RequestType -> Response
createOkResponse reqtype = Response defaultVersion (Ok reqtype) Nothing

-- | Creates an ERROR response with the given error code, default GNTP version,
-- and no encryption.
createErrorResponse :: ErrorCode -> Response
createErrorResponse errcode = Response defaultVersion (Error errcode Nothing) Nothing

-- | Creates an ERROR response with the given error code, error description,
-- default GNTP version, and no encryption.
createErrorResponseWithDescription :: ErrorCode -> ByteString -> Response
createErrorResponseWithDescription errcode errdesc =
    Response defaultVersion (Error errcode $ Just errdesc) Nothing

{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
-- |
-- Module:      Network.GNTP
-- Copyright:   (c) 2012 James Brotchie
-- Maintainer:  James Brotchie <brotchie@gmail.com>
-- Stability:   experimental
--
module Network.GNTP 
    (
    -- * Core GNTP types
      Request (..)
    , Version (..)
    , RequestType (..)
    , EncryptionAlgorithm (..)
    , Response (..)
    , ResponseType (..)
    -- * Header types
    , Header (..)
    , StringValue, IntValue, BoolValue
    , UniqueID (..), URL (..)
    , Headers, NotificationHeaders
    -- * Request helpers
    , requestType
    -- * Request parsing
    , parseRequest
    -- * Response encoding
    , encodeResponse
    , createOkResponse
    , defaultVersion
    ) where

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
                 deriving Show

-- | GNTP protocol version. Only known version is @Version 1 0@.
data Version = Version Int Int 
             deriving Show

-- | The default GNTP version.
defaultVersion :: Version
defaultVersion = Version 1 0

data Response = Response Version ResponseType (Maybe EncryptionAlgorithm)
              deriving Show
data ResponseType = Ok RequestType
                  | Error Int
                  deriving Show


-- | Not implemented as yet.
data EncryptionAlgorithm = EncryptionAlgorithm 
                         deriving Show

type StringValue = ByteString
type IntValue = Int
type BoolValue = Bool
newtype UniqueID = UniqueID ByteString deriving Show
newtype URL = URL ByteString deriving Show

data ConnectionDirective = Close | KeepAlive deriving Show

data Header = ApplicationName StringValue
            | ApplicationIcon (Either UniqueID URL)

            | OriginPlatformVersion StringValue
            | OriginSoftwareVersion StringValue
            | OriginMachineName StringValue
            | OriginSoftwareName StringValue
            | OriginPlatformName StringValue

            | NotificationsCount Int
            | NotificationName StringValue
            | NotificationDisplayName StringValue
            | NotificationEnabled BoolValue
            | NotificationIcon (Either UniqueID URL)
            | NotificationText StringValue
            | NotificationTitle StringValue

            | UnknownHeader ByteString StringValue

            | Connection ConnectionDirective
            deriving Show

type Headers = [Header]
type NotificationHeaders = [Header]

requestType :: Request -> RequestType
requestType (Request _ reqtype _ _ _) = reqtype

-- | Encodes a 'Response' instance into a lazy 'LBS.ByteString'.
encodeResponse :: Response -> LBS.ByteString
encodeResponse (Response version resptype encrypt) = runPut $ do
    put "GNTP/"
    versionEncoder version
    putSpace
    case resptype of
        (Ok reqtype) -> do put (responseTypeByteString resptype) >> putSpace
                           encryptionAlgorithmEncoder encrypt >> putCRLF
                           put "Response-Action: "
                           put (requestTypeByteString reqtype)
                           putEnd
        (Error _)    -> error "Not implemented"

-- Helpers for encoding messages.
put :: ByteString -> Put
put = putByteString

putNum :: (Num a, Show a) => a -> Put
putNum x = put . pack $ show x

putSpace :: Put
putSpace = put " "

putCRLF :: Put
putCRLF = put crlf

putEnd :: Put
putEnd = putCRLF >> putCRLF


class HeaderValue a where
    headerValueParser :: Parser a

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
responseTypeByteString (Error _) = "-ERROR"

encryptionAlgorithmEncoder :: (Maybe EncryptionAlgorithm) -> Put
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

instance HeaderValue StringValue where
    headerValueParser = takeTillChar '\r' <* eatCRLF <?> "StringValue"

instance (HeaderValue a, HeaderValue b) => HeaderValue (Either a b) where
    headerValueParser = eitherP headerValueParser headerValueParser <?> "Either"

instance HeaderValue URL where
    headerValueParser = URL <$> headerValueParser <?> "URL"

instance HeaderValue UniqueID where
    headerValueParser = string "//" >> UniqueID <$> headerValueParser <?> "UniqueID"

instance HeaderValue IntValue where
    headerValueParser = decimal <* eatCRLF <?> "IntValue"

instance HeaderValue ConnectionDirective where
    headerValueParser = do value <- (string "Close" <|> string "Keep-Alive") <* eatCRLF
                           return $ if value == "Close" then Close else KeepAlive

instance HeaderValue BoolValue where
    headerValueParser = do value <- (string "True" <|> string "False") <* eatCRLF
                           return $ value == "True"

buildHeader :: (HeaderValue a) => (a -> Header) -> Parser Header
buildHeader = (<$> headerValueParser)

headerParser :: Parser Header
headerParser = do
    headerName <- headerNameParser
    case headerName of
        "Connection"                -> buildHeader Connection
        "Application-Name"          -> buildHeader ApplicationName
        "Application-Icon"          -> buildHeader ApplicationIcon

        "Origin-Platform-Version"   -> buildHeader OriginPlatformVersion
        "Origin-Software-Version"   -> buildHeader OriginSoftwareVersion
        "Origin-Machine-Name"       -> buildHeader OriginMachineName
        "Origin-Software-Name"      -> buildHeader OriginSoftwareName
        "Origin-Platform-Name"      -> buildHeader OriginPlatformName

        "Notifications-Count"       -> buildHeader NotificationsCount
        "Notification-Enabled"      -> buildHeader NotificationEnabled
        "Notification-Name"         -> buildHeader NotificationName
        "Notification-Display-Name" -> buildHeader NotificationDisplayName
        "Notification-Icon"         -> buildHeader NotificationIcon
        "Notification-Text"         -> buildHeader NotificationText
        "Notification-Title"        -> buildHeader NotificationTitle
        _                           -> buildHeader $ UnknownHeader headerName

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

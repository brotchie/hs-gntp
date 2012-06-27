{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, PostfixOperators #-}
module Network.GNTP 
    (
      Request (..)
    , Response (..)
    , RequestType (..)
    , ResponseType (..)
    , Info (..)
    , EncryptionAlgorithm (..)
    , StringValue, IntValue, BoolValue
    , UniqueID (..), URL (..)
    , parseRequest
    , requestType
    , encodeResponse
    ) where

import Control.Applicative

import Data.Binary.Put
import Data.ByteString.Char8 hiding (map, count)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Attoparsec.Char8 hiding (parse, Result)
import Data.Attoparsec.Lazy (parse, Result)

data RequestType = Register | Notify | Subscribe deriving Show
data ResponseType = Ok RequestType
                  | Error Int
                  deriving Show

data Info = GNTP Version RequestType (Maybe EncryptionAlgorithm)
          deriving Show

data EncryptionAlgorithm = EncryptionAlgorithm deriving Show

type StringValue = ByteString
type IntValue = Int
type BoolValue = Bool
newtype UniqueID = UniqueID ByteString deriving Show
newtype URL = URL ByteString deriving Show

data ConnectionDirective = Close | KeepAlive deriving Show

data Version = Version Int Int deriving Show

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

data Request = Request Info Headers [NotificationHeaders] deriving Show
data Response = Response ResponseType deriving Show

requestType :: Request -> RequestType
requestType (Request (GNTP _ reqtype _) _ _) = reqtype

encodeResponse :: Response -> LBS.ByteString
encodeResponse (Response resptype) = runPut $
    put "GNTP/1.0 " >>
    case resptype of
        (Ok reqtype) -> put "-OK NONE" \\
                        put "Response-Action: " >>
                        put (requestTypeByteString reqtype) \\
                        done

        (Error _)    -> error "Not implemented"

    where put = putByteString
          done = put crlf
          (\\) x y = x >> put crlf >> y
          
    
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
requestTypeParser = do messageType <- takeTill isSpace <* space
                       case messageType of
                         "REGISTER"  -> return Register
                         "NOTIFY"    -> return Notify
                         "SUBSCRIBE" -> return Subscribe
                         _           -> error $ "Invalid message type " ++ unpack messageType
                         

requestTypeByteString :: RequestType -> ByteString
requestTypeByteString Register  = "REGISTER"
requestTypeByteString Notify    = "NOTIFY"
requestTypeByteString Subscribe = "SUBSCRIBE"

encryptionAlgorithmParser :: Parser (Maybe EncryptionAlgorithm)
encryptionAlgorithmParser = string "NONE" >> return Nothing

versionParser :: Parser Version
versionParser = Version <$> decimal <* char '.'
                        <*> decimal <*  space

infoParser :: Parser Info
infoParser = string "GNTP/" >>
             GNTP <$> versionParser
                  <*> requestTypeParser
                  <*> encryptionAlgorithmParser
                  <*  endOfLine

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
requestParser = do info <- infoParser <?> "Info"
                   headers <- headersParser <?> "Headers"
                   notifications <- count (notificationsCount headers) headersParser <?> "Notifications"
                   return $ Request info headers notifications

parseRequest :: LBS.ByteString -> Result Request
parseRequest = parse requestParser

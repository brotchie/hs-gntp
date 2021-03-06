{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.GNTP
import qualified Data.ByteString.Lazy.Char8 as LBS

exampleGNTPTransaction :: IO LBS.ByteString
exampleGNTPTransaction = LBS.readFile "tests/data/register.example"

main :: IO ()
main = do contents <- exampleGNTPTransaction
          print $ parseRequest contents
          LBS.putStrLn $ encodeResponse $ createOkResponse Notify
          LBS.putStrLn $ encodeResponse $ createErrorResponse 200
          LBS.putStrLn $ encodeResponse $ createErrorResponseWithDescription 200 "Bad Error"

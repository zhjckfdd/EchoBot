{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Simple (getResponseBody, httpBS, parseRequestThrow_)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.UTF8 (toString)
import Data.Aeson (decodeStrict)
import Parsing
import Utils

main :: IO ()
main = do
  config <- parseConfig
  case findKey "token" config of
    Nothing -> putStrLn "Couldn't parse token from config"
    Just token -> botLoop 0 token

sendMessage :: String -> UserMessage -> IO ()
sendMessage token (UserMessage msg _ chatId) = do
  httpBS (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token ++ "/sendMessage?chat_id=" ++ (show chatId) ++ "&text=" ++ msg)
  pure ()

botLoop :: Int -> String -> IO () 
botLoop offset token = do
  response <- httpBS (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token ++ "/getUpdates?offset=" ++ (show offset))
  let mbTgRes = decodeStrict $ getResponseBody response :: Maybe TelegramResult
  case mbTgRes of
    Nothing -> putStrLn $ "Invalid Json"
    Just tgRes -> do
      let userMessages = result tgRes
      let updateIds =  map updateId userMessages
      let newOffset = if null updateIds then 0 else maximum updateIds + 1
      mapM_ (sendMessage token) userMessages
      botLoop newOffset token

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs)
  | key == k = Just v
  | otherwise = findKey key xs

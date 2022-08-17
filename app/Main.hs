{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Conduit (RequestBody (RequestBodyBS))
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequestThrow_, setRequestMethod, setRequestBody, addRequestHeader)
import qualified Data.ByteString.Char8 as BS (pack, putStrLn)
import Data.Aeson (decodeStrict)
import Parsing
import Utils
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class (lift)



data Config = Config {token :: String, help :: String, repeat' :: Int}

main :: IO ()
main = do
  config <- parseConfig
  case findKey "token" config of
    Nothing -> putStrLn "Couldn't parse token from config"
    Just token -> case findKey "/help" config of 
      Nothing -> putStrLn "Couldn't parse /help from config"
      Just help -> case findKey "/repeat" config of
        Nothing -> putStrLn "Couldn't parse /repeat from config"
        Just repeat' -> runReaderT (botLoop 0 [] []) (Config token help (read repeat' :: Int))
  
sendMessage :: String -> UserMessage -> Int -> IO ()
sendMessage _ _ 0 = pure ()
sendMessage token (TextMessage msg update chatId) repeat' = do
  let requestMethod = setRequestMethod (BS.pack "POST") $ parseRequestThrow_ $ ("https://api.telegram.org/bot" ++ token ++ "/sendMessage")
  let requestBody = setRequestBody (RequestBodyBS $ BS.pack ("{\"chat_id\":" ++ (show chatId) ++ ",\"text\":\"" ++ msg ++ "\"}")) requestMethod
  let json = addRequestHeader "Content-Type" "application/json" requestBody
  httpBS json
  sendMessage token (TextMessage msg update chatId) (repeat' - 1)

sendSticker :: String -> FileId -> ChatId -> Int -> IO ()
sendSticker _ _ _ 0 = pure ()
sendSticker token fileId chatId repeat' = do
  let requestMethod = setRequestMethod (BS.pack "POST") $ parseRequestThrow_ $ ("https://api.telegram.org/bot" ++ token ++ "/sendSticker")
  let requestBody = setRequestBody (RequestBodyBS $ BS.pack ("{\"chat_id\":" ++ (show chatId) ++ ",\"sticker\":\"" ++ fileId ++ "\"}")) requestMethod
  let json = addRequestHeader "Content-Type" "application/json" requestBody
  httpBS json
  sendSticker token fileId chatId (repeat' - 1)

sendKeyboard :: String -> TextMsg -> UpdateId -> ChatId -> IO ()
sendKeyboard token msg update chatId = do
  let requestMethod = setRequestMethod (BS.pack "POST") $ parseRequestThrow_ $ ("https://api.telegram.org/bot" ++ token ++ "/sendMessage")
  let requestBody = setRequestBody (RequestBodyBS $ BS.pack ("{\"chat_id\":" ++ (show chatId) ++ ",\"text\":\"" ++ msg ++ "\",\"reply_markup\":{\"keyboard\":[[{\"text\":1},{\"text\":2},{\"text\":3},{\"text\":4},{\"text\":5}]]}}")) requestMethod
  let json = addRequestHeader "Content-Type" "application/json" requestBody
  httpBS json
  pure ()

removeKeyboard :: String -> TextMsg -> UpdateId -> ChatId -> IO ()
removeKeyboard token msg update chatId = do
  let requestMethod = setRequestMethod (BS.pack "POST") $ parseRequestThrow_ $ ("https://api.telegram.org/bot" ++ token ++ "/sendMessage")
  let requestBody = setRequestBody (RequestBodyBS $ BS.pack ("{\"chat_id\":" ++ (show chatId) ++ ",\"text\":\"" ++ msg ++ "\",\"reply_markup\":{\"remove_keyboard\":true}}")) requestMethod
  let json = addRequestHeader "Content-Type" "application/json" requestBody
  httpBS json
  pure ()

botLoop :: Int -> [Int] -> [(Int, Int)] -> ReaderT Config IO () 
botLoop offset acc acc2 = do
  cfg <- ask
  response <- lift $ httpBS (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token cfg ++ "/getUpdates?offset=" ++ (show offset))
  let mbTgRes = decodeStrict $ getResponseBody response :: Maybe TelegramResult
  case mbTgRes of
    Nothing -> lift $ putStrLn $ "Invalid Json"
    Just tgRes -> do
      let userMessages = result tgRes
      let updateIds = map getUpdId userMessages
      let newOffset = if null updateIds then 0 else maximum updateIds + 1
      (allRepeat, userSettings) <- handleUpdates userMessages acc acc2
      botLoop newOffset allRepeat userSettings

handleUpdates :: [UserMessage] -> [Int] -> [(Int, Int)] -> ReaderT Config IO ([Int], [(Int, Int)])
handleUpdates [] acc acc2 = pure (acc, acc2)
handleUpdates (x:xs) acc acc2 = do
  cfg <- ask
  case x of
    TextMessage text updId chatId -> 
      case text of
        "/help" -> do
          lift $ sendMessage (token cfg) (TextMessage (help cfg) updId chatId) 1
          handleUpdates xs acc acc2
        "/repeat" -> do
          lift $ sendKeyboard (token cfg) ("Tell me, how much times I should repeat your messages. From 1 to 5:") updId chatId
          handleUpdates xs (chatId:acc) acc2
        _ -> 
          if chatId `elem` acc
            then do
              let userSettings = (chatId, read text :: Int):acc2
              lift $ removeKeyboard (token cfg) ("Ok, Now I will repeat your messages " ++ text ++ " times") updId chatId
              let newAcc = deleteFromList chatId acc []
              handleUpdates xs newAcc userSettings
            else do
              case lookup chatId acc2 of
                Just x -> lift $ sendMessage (token cfg) (TextMessage text updId chatId) x
                Nothing -> lift $ sendMessage (token cfg) (TextMessage text updId chatId) (repeat' cfg)
              handleUpdates xs acc acc2        
    StickerMessage fileId _ chatId -> do
      case lookup chatId acc2 of
        Just x -> lift $ sendSticker (token cfg) fileId chatId x
        Nothing -> lift $ sendSticker (token cfg) fileId chatId (repeat' cfg)
      handleUpdates xs acc acc2

getUpdId :: UserMessage -> Int
getUpdId (TextMessage _ updateId _ ) = updateId
getUpdId (StickerMessage _ updateId _ ) = updateId

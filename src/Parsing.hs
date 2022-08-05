{-# LANGUAGE OverloadedStrings #-}

module Parsing where 

import Control.Applicative ((<|>))
import Data.Aeson
import Control.Monad (mzero)

data TelegramResult = TelegramResult {result :: [UserMessage]}

instance FromJSON TelegramResult where
  parseJSON (Object json) = do
    result <- json .: "result"
    pure (TelegramResult result)
  parseJSON _ = mzero

type ChatId = Int
type UpdateId = Int
type TextMsg = String
type FileId = String

data UserMessage = 
  TextMessage TextMsg UpdateId ChatId | 
  StickerMessage FileId UpdateId ChatId

instance FromJSON UserMessage where
  parseJSON (Object result) = parseText <|> parseSticker
    where
      parseText = do
        message <- result .: "message"
        from <- message .: "from"
        userId <- from .: "id"
        update <- result .: "update_id"
        text <- message .: "text"
        pure (TextMessage text update userId)
      parseSticker = do
        update <- result .: "update_id"
        message <- result .: "message"
        from <- message .: "from"
        userId <- from .: "id"
        sticker <- message .: "sticker"
        fileId <- sticker .: "file_id"
        pure (StickerMessage fileId update userId)
  parseJSON _ = mzero

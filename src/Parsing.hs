{-# LANGUAGE OverloadedStrings #-}

module Parsing where 

import Data.Aeson
import Control.Monad (mzero)

data TelegramResult = TelegramResult {result :: [UserMessage]}

instance FromJSON TelegramResult where
  parseJSON (Object json) = do
    result <- json .: "result"
    pure (TelegramResult result)
  parseJSON _ = mzero

data UserMessage = UserMessage {text :: String, updateId :: Int, userId :: Int}

instance FromJSON UserMessage where
  parseJSON (Object result) = do
    message <- result .: "message"
    from <- message .: "from"
    userId <- from .: "id"
    update <- result .: "update_id"
    text <- message .: "text"
    pure (UserMessage text update userId)
  parseJSON _ = mzero

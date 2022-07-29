module Utils where

tokenFile :: String
tokenFile = "bot.cfg"

parseConfig :: IO [(String, String)]
parseConfig = do
  contents <- readFile tokenFile
  let strings = lines contents
  let end = map parseToken strings
  pure end

parseToken :: String -> (String, String)
parseToken s = go s []
  where
    go [] acc = ([], [])
    go (x:xs) acc =
      if x == '='
        then (reverse acc, xs)
        else go xs (x:acc)

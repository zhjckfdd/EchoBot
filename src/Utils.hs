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

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs)
  | key == k = Just v
  | otherwise = findKey key xs

deleteFromList :: (Eq a) => a -> [a] -> [a] -> [a]
deleteFromList _ [] acc = acc
deleteFromList x (y:ys) acc =
  if x == y
    then acc ++ ys
    else deleteFromList x ys (y:acc)
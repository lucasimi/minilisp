module Utils where

-- drop all blank leading characters from a string
dropLeadingBlanks :: String -> String
dropLeadingBlanks = dropWhile (`elem` " \t\n\r")

-- check is a string has only blank characters
isBlank :: String -> Bool
isBlank str = dropLeadingBlanks str == ""

-- splitOnDelimiter a string where a delimiter is found
splitOnDelimiter :: String -> (String, String)
splitOnDelimiter = (break (`elem` " \t\n\r()."))

split :: String -> (String, String)
split = splitOnDelimiter . dropLeadingBlanks

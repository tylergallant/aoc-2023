module Utils.Parsing where

import Data.Maybe (listToMaybe)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S, readS_to_P)

parseReadable :: Read a => ReadP a
parseReadable = readS_to_P reads

runParser :: ReadP a -> String -> Maybe a
runParser p = listToMaybe . fmap fst . filter (null.snd) . readP_to_S p

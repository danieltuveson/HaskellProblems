module Hw2.LogAnalysis where

import Hw2.Log 
import Text.Read (readMaybe)

parse :: String -> [LogMessage]
parse = map parseMessage . lines 

parseMessage :: String -> LogMessage
parseMessage = parseWords . words

parseWords :: [String] -> LogMessage
parseWords line@("E" : errNum : tStamp : strs) =
            case (readMaybe errNum :: Maybe Int, readMaybe tStamp :: Maybe Int) of 
                (Just en, Just ts) -> LogMessage (Error en) ts (unwords strs)
                _                  -> Unknown (unwords line)
parseWords line@(mType : tStamp : strs) =
            case readMaybe tStamp :: Maybe Int of 
                Just ts -> LogMessage mt ts (unwords strs)
                    where mt 
                             | mType == "I" = Info 
                             | otherwise    = Warning
                Nothing -> Unknown (unwords line)
parseWords line = Unknown (unwords line)


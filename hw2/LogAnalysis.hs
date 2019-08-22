module Hw2.LogAnalysis where

import Hw2.Log 
import Text.Read (readMaybe)

-- LOG PARSER: 

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
                    where mt | mType == "I" = Info 
                             | otherwise    = Warning
                Nothing -> Unknown (unwords line)
parseWords line = Unknown (unwords line)


-- Tree stuff

-- data MessageTree = Leaf
--                  | Node MessageTree LogMessage MessageTree
--   deriving (Show, Eq)

-- data LogMessage = LogMessage MessageType TimeStamp String
--                 | Unknown String
--                 deriving (Show, Eq) 

-- Inserts LogMessage into a sorted (by timestamp) MessageTree
insert :: LogMessage -> MessageTree -> MessageTree 
insert msg Leaf      = Node Leaf msg Leaf
insert msg@(LogMessage _ msgTs _) (Node lt rootMsg@(LogMessage _ rootTs _) rt)
    | msgTs < rootTs = Node (insert msg lt) rootMsg rt
    | msgTs > rootTs = Node lt rootMsg (insert msg rt)
insert _ t           = t


-- Builds a MessageTree from a list of LogMessages 
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Takes a MessageTree sorted by TimeStamp and returns a list of logs 
-- sorted by TimeStamp
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt msg rt) = inOrder lt ++ [msg] ++ inOrder rt


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = let sortedMsgs = sortMsgs msgs
                         errMsgs = map getErrMsg sortedMsgs
                         errMsgsCleanedUp = filter (/= "") errMsgs
                     in errMsgsCleanedUp

getErrMsg :: LogMessage -> String
getErrMsg (LogMessage (Error num) _ msg)
    | num >= 50 = msg
getErrMsg _     = ""



sortMsgs :: [LogMessage] -> [LogMessage]
sortMsgs = inOrder . build

whodunnit :: IO [String]
whodunnit = testWhatWentWrong parse whatWentWrong "Hw2/error.log"












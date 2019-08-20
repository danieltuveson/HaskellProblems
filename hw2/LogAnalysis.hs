module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage str = Unknown str


messageType :: String -> Maybe (MessageType, String)
messageType ('I' : ' ' : str) = Just (Info, str)
messageType ('W' : ' ' : str) = Just (Warning, str)
messageType ('E' : ' ' : str) = Nothing

messageType s = Nothing


-- isInt :: String -> Int
-- isInt str = foldr isDigit False str
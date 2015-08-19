{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage string = case words string of
  ("I":t:msg)          -> LogMessage Info (read t) (unwords msg)
  ("W":t:msg)          -> LogMessage Warning (read t) (unwords msg)
  ("E":severity:t:msg) -> LogMessage (Error (read severity)) (read t) (unwords msg)
  _                    -> Unknown string

parse :: String -> [LogMessage]
parse string = map parseMessage $ lines string

insert :: LogMessage -> MessageTree -> MessageTree
insert message Leaf = Node Leaf message Leaf
insert m1@(LogMessage _ t1 _) (Node l m2@(LogMessage _ t2 _) r)
  | t1 > t2   = Node l m2 (insert m1 r)
  | otherwise = Node (insert m1 l) m2 r
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left root right) = inOrder left ++ [root] ++ inOrder right

severe :: LogMessage -> Bool
severe (LogMessage (Error i) _ _) = i > 50
severe _ = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ message) = message
getMessage (Unknown message) = message

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . inOrder . build . filter severe

testMessages :: [LogMessage]
testMessages = [
  LogMessage Info 6 "Completed armadillo processing",
  LogMessage Info 1 "Nothing to report",
  LogMessage Info 4 "Everything normal",
  LogMessage Info 11 "Initiating self-destruct sequence",
  LogMessage (Error 70) 3 "Way too many pickles",
  LogMessage (Error 65) 8 "Bad pickle-flange interaction detected",
  LogMessage Warning 5 "Flange is due for a check-up",
  LogMessage Info 7 "Out for lunch, back in two time steps",
  LogMessage (Error 20) 2 "Too many pickles",
  LogMessage Info 9 "Back from lunch",
  LogMessage (Error 99) 10 "Flange failed!"]

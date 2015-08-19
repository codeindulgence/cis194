{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage string = case words string of
  ("I":t:msg)          -> LogMessage Info (read t) (unwords msg)
  ("W":t:msg)          -> LogMessage Warning (read t) (unwords msg)
  ("E":t:severity:msg) -> LogMessage (Error (read severity)) (read t) (unwords msg)
  _                    -> Unknown string

parse :: String -> [LogMessage]
parse string = map parseMessage $ lines string

insert :: LogMessage -> MessageTree -> MessageTree
insert message Leaf = Node Leaf message Leaf
insert m1@(LogMessage _ t1 _) (Node l m2@(LogMessage _ t2 _) r)
  | t1 > t2   = Node l m2 (insert m1 r)
  | otherwise = Node (insert m1 l) m2 r
insert _ tree = tree

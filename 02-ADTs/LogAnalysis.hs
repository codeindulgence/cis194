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

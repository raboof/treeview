module Hierarchy where

import Data.Tree
import Data.List
import Data.Char

filterModelForest :: (Forest String) -> String -> (Forest String)
filterModelForest [] _ = []
filterModelForest (x:xs) filter 
  | isInfixOf filter (rootLabel x) = (x : filterModelForest xs filter)
  | matchingSubtree /= [] = (Node (rootLabel x) matchingSubtree : filterModelForest xs filter)
  | otherwise = filterModelForest xs filter
  where 
    matchingSubtree = filterModelForest (subForest x) filter

isBlank :: String -> Bool
isBlank [] = True
isBlank (x:xs)
    | isSpace x = isBlank xs
    | otherwise = False

removeBlank :: [String] -> [String]
removeBlank = filter (not . isBlank)

indentationOf :: String -> Int
indentationOf [] = 0
indentationOf (x:xs)
    | isSpace x = 1 + indentationOf xs
    | otherwise = 0

isChildOf :: String -> String -> Bool
isChildOf parent child = (indentationOf child) > (indentationOf parent)

data Splitlist a = Splitlist [a] [a]

splitOnFirstFailure :: (a -> Bool) -> [a] -> (Splitlist a)
splitOnFirstFailure condition [] = Splitlist [] []
splitOnFirstFailure condition (x:xs)
  | condition x = Splitlist (x:otherMatches) nonMatches
  | otherwise = Splitlist [] (x:xs)
    where 
      Splitlist otherMatches nonMatches = splitOnFirstFailure condition xs

parseForestLines :: [String] -> Forest String
parseForestLines [] = []
parseForestLines (x:xs) = [Node x (parseForestLines subNodes)] ++ parseForestLines otherNodes
  where
    Splitlist subNodes otherNodes = splitOnFirstFailure (isChildOf x) xs

parseForest :: String -> Forest String
parseForest file = parseForestLines (removeBlank (lines file))



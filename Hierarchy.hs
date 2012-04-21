module Hierarchy where

import Data.Tree
import Data.List

filterModelForest :: (Forest String) -> String -> (Forest String)
filterModelForest [] _ = []
filterModelForest (x:xs) filter 
  | isInfixOf filter (rootLabel x) = (x : filterModelForest xs filter)
  | matchingSubtree /= [] = (Node (rootLabel x) matchingSubtree : filterModelForest xs filter)
  | otherwise = filterModelForest xs filter
  where 
    matchingSubtree = filterModelForest (subForest x) filter

isBlankChar :: Char -> Bool
isBlankChar '\n' = True
isBlankChar '\r' = True
isBlankChar ' '  = True
isBlankChar '\t' = True
isBlankChar _ = False

isBlank :: String -> Bool
isBlank [] = True
isBlank (x:xs)
    | isBlankChar x = isBlank xs
    | otherwise = False

removeBlank :: [String] -> [String]
removeBlank [] = []
removeBlank (x:xs)
    | isBlank x = removeBlank xs
    | otherwise = (x : removeBlank xs)

indentationOf :: String -> Int
indentationOf [] = 0
indentationOf (x:xs)
    | isBlankChar x = 1 + indentationOf xs
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



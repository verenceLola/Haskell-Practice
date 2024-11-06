module DataChar where

import Data.Char

toUpperCase ::  Char -> Char
toUpperCase = toUpper

transToUpper ::  String -> String
transToUpper = filter isUpper

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs

capitalize' :: String -> String
capitalize' (x:xs) = toUpper x : capitalize' xs
capitalize' [] = []

capitalizeWithHead :: String -> Char
capitalizeWithHead = head

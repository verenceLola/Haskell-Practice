module PatternMatching where

myTail :: [a] -> [a]
myTail [] = []
myTail (_ : xs) = xs

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail [x] = Nothing
safeTail (_ : xs) = Just xs


-- List Comprehension
mySqr = [x^2 | x <- [1..5]]

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x: xs) = x + mySum xs

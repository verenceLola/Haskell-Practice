-- typeInference1.hsLibraries
module TypeInference1 where

f :: Num a => a -> a -> a
f x y = x + y + 3

comp :: Ord a => a -> a -> a -> a
comp x y z = max x (max y z)
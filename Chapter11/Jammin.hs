module Jammin where

data Fruit = Peach | Plum | Apple | Blackberry deriving (Eq, Ord, Show)

data JamJars = Jam
  { fruit :: Fruit,
    jars :: Int
  }
  deriving (Eq, Ord, Show)

row1 = Jam Peach 11

row2 = Jam Apple 10

allJars = [row1, row2]

numOfJars :: [JamJars] -> [Int]
numOfJars = map getJars
  where
    getJars (Jam _ jars) = jars

totalJars :: [JamJars] -> Int
totalJars = sum . numOfJars

mostRow :: [JamJars] -> JamJars
mostRow (x : xs) = foldl1 (\acc y -> if jars x > jars y then x else y) xs

data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType = FictionBook Fiction | NonfictionBook Nonfiction deriving Show

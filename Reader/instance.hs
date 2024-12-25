module ReaderChapter where
{-# LANGUAGE InstanceSigs #-}

import           Control.Applicative
import           Control.Monad.Reader (Reader)
import           Data.Char

hurr = (*2)
durr = (+10)

m :: Integer -> Integer
m = hurr . durr

m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> durr

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr

hurrDurr :: Integer -> Integer
hurrDurr = do
    a <- hurr
    b <- durr
    return (a + b)

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupledDo :: [Char] -> ([Char], [Char])
tupledDo = do
    c <- cap
    r <- rev
    return (c, r)

tupledDo' :: [Char] -> ([Char], [Char])
tupledDo' = do
    rev >>=
        \x -> cap >>=
            (,)

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person {
    humanName :: HumanName
,   dogName   :: DogName
,   address   :: Address
} deriving (Eq, Show)

data Dog = Dog {
    dogsName    :: DogName
,   dogsAddress :: Address
} deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader ra) = Reader (f . ra)

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ undefined a

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader rab) <*> (Reader ra) = Reader (rab . ra)
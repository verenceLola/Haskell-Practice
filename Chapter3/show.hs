module TypeClasses where

data Trivial = Trivial'

instance Eq Trivial where
  (==) Trivial' Trivial' = True

data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun deriving (Show)

data Date = Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') = weekday == weekday' && dayOfMonth == dayOfMonth'

-- Identity

data Indentity a = Indentity a

instance (Eq a) => Eq (Indentity a) where
  (==) (Indentity v) (Indentity v') = v == v'

-- instance Ord a => Eq (Indentity a) where
--     (==) (Indentity v) (Indentity v') = compare v v' == EQ

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn i) (TisAn i') = i == i'

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two i1 i2) (Two i1' i2') = i1 == i1' && i2 == i2'

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt a') = a == a'
  (==) (TisAString s) (TisAString s') = s == s'
  (==) _ _ = False

instance Ord DayOfWeek where
    compare Fri Fri = EQ
    compare Fri _ = GT
    compare _ Fri = LT
    compare _ _ = EQ


data Rocks = Rocks String deriving (Eq, Show)

data Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)

true = Papu (Rocks "chases") (Yeah True)
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

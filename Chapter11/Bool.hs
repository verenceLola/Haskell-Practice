{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module DataTypes where

data PugType = PugData

data HuskyType a = HuskyData

newtype DogueDeBordeaux doge = DogueDeBordeaux doge

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHisky :: (Num a) => HuskyType a
myOtherHisky = HuskyData

myOtherOtherHusky :: HuskyType [[[[[[Int]]]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

data Doggies a = Husky a | Mastiff a deriving (Eq, Show)

newtype Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

newtype PlaneSize = PlaneSize Integer deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane PlaneSize Airline deriving (Eq, Show)

myCar = Car Mini (Price 14000)

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane (PlaneSize 10) PapuAir

isCar :: Vehicle -> Bool
isCar x = case x of
  Car _ _ -> True
  _ -> False

isPlane :: Vehicle -> Bool
isPlane y = case y of
  Plane _ _ -> True
  _ -> False

areCars :: [Vehicle] -> [Bool]
areCars = foldr isACar []
  where
    isACar a b = if isCar a then True : b else b

getManu :: Vehicle -> Maybe Manufacturer
getManu y = case y of
  (Car manufacturer _) -> Just manufacturer
  _ -> Nothing

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

newtype Cows = Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany = (>) 42

instance TooMany String where
  tooMany s = length s > 42

instance TooMany (Int, String) where
  tooMany (a, a') = tooMany (a + length a')

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (a, a') = tooMany (a + a')

data Person = MkPerson String Int deriving (Eq, Show)

jm = MkPerson "julie" 108

ca = MkPerson "chris" 16

namae :: Person -> String
namae (MkPerson s _) = s

data Person' = Person' {name :: String, age :: Int} deriving (Eq, Show)

data GuessWhat = Chickenbutt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a | Second b deriving (Eq, Show)

data RecordProduct a b = RecordProduct
  { pfirst :: a,
    psecond :: b
  }
  deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)

newtype NumPig = NumPig Int deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving (Eq, Show)

data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

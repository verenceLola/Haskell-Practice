module DeconstructingValues where

newtype Name = Name String deriving (Show)

newtype Acres = Acres Int deriving (Show)

data FarmerType = DairyFarmer | WheatFarmer | SoybeanFarmer deriving (Show)

data Farmer = Farmer Name Acres FarmerType deriving (Show)

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec = FarmerRec {name :: Name, acres :: Acres, farmerType :: FarmerType} deriving (Show)

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = case farmerType farmer of
  DairyFarmer -> True
  _ -> False

data Quatum = Yes | No | Both deriving (Eq, Show)

quatumSum1 :: Either Quatum Quatum
quatumSum1 = Right Yes

quatumSum2 :: Either Quatum Quatum
quatumSum2 = Right No

quatumSum3 :: Either Quatum Quatum
quatumSum3 = Right Both

quatumSum4 :: Either Quatum Quatum
quatumSum4 = Left Yes

quatumSum5 :: Either Quatum Quatum
quatumSum5 = Left No

quatumSum6 :: Either Quatum Quatum
quatumSum6 = Left Both

convert :: Quatum -> Bool
convert = undefined

data Product a b = a :&: b deriving (Eq, Show)

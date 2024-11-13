module Maybe where

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n + 2) else Nothing

type Name = String

type Age = Integer

data Person = Person Name Age deriving (Show)

data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

mkPerson :: Name -> Integer -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0 = Just $ Person name age
  | otherwise = Nothing

mkPerson' :: Name -> Integer -> Either PersonInvalid Person
mkPerson' name age
  | name /= "" && age >= 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | otherwise = Left AgeTooLow

type ValidatePerson a = Either [PersonInvalid] a

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = if age >= 0 then Right age else Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = if name /= "" then Right name else Left [NameEmpty]

mkPersonValid :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPersonValid (Right nameOk) (Right ageOk) = Right $ Person nameOk ageOk
mkPersonValid (Left badName) (Left badAge) = Left $ badName ++ badAge
mkPersonValid (Left badName) _ = Left badName
mkPersonValid _ (Left badAge) = Left badAge

mkPerson'' :: Name -> Age -> ValidatePerson Person
mkPerson'' name age = mkPersonValid (nameOkay name) $ ageOkay age
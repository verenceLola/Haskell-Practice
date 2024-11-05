module RegisteredUser where

newtype Username = Username String

newtype AccountNumber = AccountNumber Integer

data User = UnregistredUser | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregistredUser = putStrLn "UnregistredUser"
printUser (RegisteredUser (Username name) (AccountNumber accountNumber)) =
  putStrLn $ name ++ " " ++ show accountNumber

data WherePenguinsLive = Galapagos | Antarctica | Australia | SouthAfrica | SouthAmerica deriving (Eq, Show)

data Penguin = Peng WherePenguinsLive deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmerWhereTheyLive :: Penguin -> WherePenguinsLive
gimmerWhereTheyLive (Peng whereilives) = whereilives

galaposPenguin :: Penguin -> Bool
galaposPenguin (Peng Galapagos) = True
galaposPenguin _ = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _ = False

antarcticOrGalapos :: Penguin -> Bool
antarcticOrGalapos p = galaposPenguin p || antarcticPenguin p

funZ x = case x + 1 == 1 of
  True -> "AWESOME"
  False -> "wut"

palindrome xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"

palindrome' xs =
  case y of
    True -> "yes"
    False -> "no"
  where
    y = xs == reverse xs

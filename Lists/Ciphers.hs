module Cipher where

import Data.Time

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)), DbNumber 9001, DbString "Hello, world!", DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 43123))]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr maybeCons []
  where
    maybeCons a b =
      case a of
        (DbDate date) -> date : b
        _ -> b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr filterNumbers []
  where
    filterNumbers a b =
      case a of
        (DbNumber num) -> num : b
        _ -> b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb x = fromIntegral (div (totalNumbers x) (lengthOfNumbers x))
  where
    totalNumbers =    sum . dbNumbers
    lengthOfNumbers = fromIntegral . length . dbNumbers
    dbNumbers = filterDbNumber



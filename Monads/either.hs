module EitherMonad where

type Founded = Int
type Coders = Int

data SoftwareShop = Shop {
    founded     :: Founded,
    programmers :: Coders
}

data FoundedError = NegativeYears Founded
    | TooManyYears Founded
    | NegavideCoders Coders
    | TooManyCoders Coders
    | TooManyCodersForYears Founded Coders
 deriving (Eq, Show)


validateFounded :: Int -> Either FoundedError Founded
validateFounded n
    | n < 0 = Left $ NegativeYears n
    | n > 500 = Left $ TooManyYears n
    | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
    | n < 0 = Left $ NegavideCoders n
    | n > 500 = Left $ TooManyCoders n
    | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
    founded <- validateFounded years
    programmers <- validateCoders coders
    if programmers > div founded 10
        then Left $ TooManyCodersForYears founded programmers
        else Right $ Shop founded programmers

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a ) = First a
    fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
    pure = Second
    (<*>) (First a) _            = First a
    (<*>) _ (First a)            = First a
    (<*>) (Second a) (Second a') = Second $ a a'

instance Monad (Sum a) where
    return = pure
    (>>=) (First a) _  = First a
    (>>=) (Second b) f = f b

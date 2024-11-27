{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module MaybeFunctors where

incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing  = Nothing

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just s) = Just (show s)
showIfJust Nothing  = Nothing

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe = fmap (+1)

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe = fmap show

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)
instance Functor Possibly where
    fmap _ LolNope     = LolNope
    fmap f (Yeppers a) = Yeppers $ f a

incEither :: Num a => Either e a -> Either e a
incEither = fmap (+1)

data Sum a b = First a | Second b deriving (Eq, Show)
instance Functor (Sum a) where
    fmap f (Second b) = Second $ f b
    fmap _ (First a)  = First a


data Wrap f a = Wrap (f a) deriving (Eq, Show)
instance Functor f => Functor (Wrap f) where
    fmap f (Wrap a) = Wrap (fmap f a)

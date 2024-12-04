module Exercise where

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap f (Cons a l) = Cons (f a) (fmap f l)
    fmap _ Nil        = Nil

instance Applicative List where
    pure a = Cons a Nil
    (<*>) (Cons _ _) Nil = Nil
    (<*>) (Cons f fs) xs = fmap f xs `append` (fs <*> xs)
        where append Nil ys         = ys
              append (Cons x xs) ys = Cons x (append xs ys)
    (<*>) _ Nil = Nil
    (<*>) Nil _ = Nil

data Sum a b = First a | Second b deriving (Eq, Show)

data Validation e a = Error e | Success a deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a)  = First a
    fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
    pure = Second
    (<*>) _ (First a)            = First a
    (<*>) (First a) _            = First a
    (<*>) (Second b) (Second b') = Second (b b')

instance Functor (Validation e) where
    fmap _ (Error e)   = Error e
    fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
    pure = Success
    (<*>) (Error a) (Error a')     = Error (a `mappend` a')
    (<*>) _ (Error a)              = Error a
    (<*>) (Error a) _              = Error a
    (<*>) (Success a) (Success a') = Success (a a')

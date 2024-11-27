module FMappingTuples where

data Two a b = Two a b deriving (Eq, Show)

data Or a b = First a | Second b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)
instance Functor (Or a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorComponse :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorComponse f g x = fmap g (fmap f x) == fmap (g . f) x

data Three a b c = Three a b c

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

data Four a b c d = Four a b c d

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)
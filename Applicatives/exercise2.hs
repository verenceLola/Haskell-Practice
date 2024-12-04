module ChapterExercise where

newtype Identity a = Identity a deriving Show

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity a) (Identity a') = Identity (a a')

data Pair a = Pair a a deriving Show

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
    pure a = Pair a a
    (<*>) (Pair a a') (Pair b b') = Pair (a b) (a' b')

data Two a b = Two a b

instance Functor (Two a) where
    fmap f (Two a b) = Two a $ f b

instance Monoid a => Applicative (Two a) where
    pure = Two mempty
    (<*>) (Two a b) (Two a' b') = Two (a <> a') (b b')

data Three a b c = Three a b c

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b)  => Applicative (Three a b) where
     pure = Three mempty mempty
     (<*>) (Three a b c) (Three a' b' c') = Three (a <> a') (b <> b') (c c')


data Three' a b = Three' a b b

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f b)

instance Monoid a => Applicative (Three' a) where
    pure b = Three' mempty b b
    (<*>) (Three' a b b') (Three' a' c c') = Three' (a <> a') (b c) (b' c')

data Four a b c d = Four a b c d

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c $ f d

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure = Four mempty mempty mempty
    (<*>) (Four a b c d) (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') $ d d'


data Four' a b = Four' a a a b

instance Functor (Four' a) where
    fmap f (Four' a a' a'' b) = Four' a a' a'' $ f b

instance Monoid a => Applicative (Four' a) where
    pure = Four' mempty mempty mempty
    (<*>) (Four' a a' a'' b) (Four' c c' c'' d) = Four' (a <> c) (a' <> c') (a'' <> c'') $ b d

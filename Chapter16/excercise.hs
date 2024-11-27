module Excercise where

data BoolAndSomethingElse a = False' a | True' a deriving (Show)

instance Functor BoolAndSomethingElse where
    fmap f (False' a) = False' $ f a
    fmap f (True' a)  = True' $ f a

newtype Mu f = InF {outF :: f (Mu f)}

data Sum a b = First a | Second b

instance Functor (Sum a) where
    fmap _ (First a)  = First a
    fmap f (Second a) = Second $ f a

data Company a b c = DeepBlue a c | Something b
instance Functor (Company a b) where
    fmap f (DeepBlue a c) = DeepBlue a  $ f c
    fmap _ (Something b)  = Something b

data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More a) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R a b a') = R a (f b) a'


data Quant a b = Finance | Desk a | Floor b
instance Functor (Quant a) where
    fmap _ Finance   = Finance
    fmap _ (Desk a)  = Desk a
    fmap f (Floor a) = Floor $ f a

newtype K a b = K a
instance Functor (K a) where
    fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
instance Functor (Flip K a) where
    fmap f (Flip (K a)) = Flip $ K $ f a

newtype EvilGoateeConst a b = GoatyConst b
instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst a) = GoatyConst $ f a

newtype LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut $ fmap f fa

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance (Functor f ,Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa  (fmap f gb)

data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga $ fmap f gt

data List a = Nil | Cons a (List a)
instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons a la) = Cons (f a) $ fmap f la

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
    fmap _ NoGoat            = NoGoat
    fmap f (OneGoat a)       = OneGoat $ f a
    fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
    fmap _ Halt        = Halt
    fmap f (Print s a) = Print s $ f a
    fmap f (Read fs)   = Read $  f . fs

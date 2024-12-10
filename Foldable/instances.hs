module Foldables where

import           Data.Foldable
import           Data.Monoid

newtype Identity a = Identity a

instance Foldable Identity where
    foldr f z (Identity x) = f x z
    foldl f z (Identity x) = f z x
    foldMap f (Identity x) = f x

data Optional a = Nada | Yep a

instance Foldable Optional where
    foldr _ z Nada    = z
    foldr f z (Yep x) = f x z

    foldl _ z Nada    = z
    foldl f z (Yep x) = f z x

    foldMap _ Nada    = mempty
    foldMap f (Yep a) = f a

newtype Constant' a b = Constant' a

instance Semigroup (Constant' a b) where
    (<>) (Constant' _) x = x

instance Monoid (Constant' a b) where
    mempty = Constant' undefined

instance Foldable (Constant' a) where
    foldMap f (Constant' _) =  mempty

data Two a b = Two a b

instance (Semigroup a, Semigroup b)  => Semigroup (Two a b) where
    (<>) (Two a b) (Two a' b') =Two (a <> a') (b' <> b)

instance (Semigroup a, Semigroup b) => Monoid (Two a b) where
    mempty = Two undefined undefined

instance Foldable (Two a) where
    foldMap f (Two _ b) = f b

data Three a b c = Three a b c

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (<>) (Three a b c) (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Semigroup a, Semigroup b, Semigroup c)=> Monoid (Three a b c) where
    mempty = Three undefined undefined undefined

instance Foldable (Three a b) where
    foldMap f (Three _ _ c) = f c

data Four a b = Four a b b b

instance (Semigroup a, Semigroup b) => Semigroup (Four a b) where
    (<>) (Four a b b' b'') (Four a' c c' c'') = Four (a <> a') (b <> c) (b' <> c') (b'' <> c'')

instance (Semigroup a, Semigroup b) => Monoid (Four a b) where
    mempty = Four undefined undefined undefined undefined

instance Foldable (Four a) where
    foldMap f (Four _ b b' b'') = f b <> f b' <> f b''

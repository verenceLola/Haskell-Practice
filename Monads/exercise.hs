module Exercise where
import           Control.Monad (ap, join)
import           Prelude       hiding (Left, Right)

data Nope a = NopeDotJpg

instance Functor Nope where
    fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    return = pure
    (>>=) NopeDotJpg _ = NopeDotJpg


data PhhEither b a = Left a | Right b

instance Functor (PhhEither b) where
    fmap f (Left a)  = Left $ f a
    fmap _ (Right b) = Right b

instance Applicative (PhhEither b) where
    pure = Left
    (<*>) _ (Right a)        = Right a
    (<*>) (Right a) _        = Right a
    (<*>) (Left a) (Left a') = Left (a a')

instance Monad (PhhEither b) where
    (>>=) (Right a) _ = Right a
    (>>=) (Left b) f  = f b

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity a) (Identity a') = Identity $ a a'

instance Monad Identity where
    (>>=) (Identity a) f = f a

data List a = Nil | Cons a (List a)

instance Semigroup (List a) where
    (<>) Nil ys          = ys
    (<>) (Cons a fl) fl' = Cons a $ fl <> fl'

instance Monoid (List a) where
  mempty = Nil


instance Functor List where
    fmap _ Nil        = Nil
    fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Applicative List where
    pure a = Cons a mempty
    (<*>) _ Nil          = Nil
    (<*>) Nil _          = Nil
    (<*>) (Cons f fs) xs = fmap f xs <> (fs <*> xs)

instance Monad List where
    (>>=) Nil _         = Nil
    (>>=) (Cons a fl) f = f  a <> (fl >>= f)


j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x: xs) f = do
        y <- f x
        ys <- meh xs f
        return (y:ys)

flipType :: (Monad m) => [m a] -> m [a]
flipType l = meh l id

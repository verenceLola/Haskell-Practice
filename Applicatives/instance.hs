module ApplicativeInstances where

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    (<*>) (Identity f) (Identity a) = Identity $ f a
    pure = Identity

newtype Constant a b = Constant {getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _= Constant mempty
  (<*>) (Constant f) (Constant a) = Constant (f `mappend` a)

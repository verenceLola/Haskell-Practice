module MonoidsAndSemiGroup where

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend Nada x = x
    mappend x Nada = x
    mappend (Only a) (Only b) = Only (mappend a b)

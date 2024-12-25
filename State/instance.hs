module StatePractise where

newtype Moi s a = Moi { runMoi :: s -> (a, s)}

instance Functor (Moi s) where
    fmap f (Moi g) = Moi $ \s -> let (a, newState) = g s in (f a, newState)
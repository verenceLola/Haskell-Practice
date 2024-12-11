module Traversables where

data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn = (traverse makeIoOnlyObj . traverse decodeFn =<<) . fetchFn

data Either' a b  = Left' a | Right' b deriving (Eq, Ord, Show)

instance Functor (Either' e) where
    fmap _ (Left' a) = Left' a
    fmap f (Right' b) = Right' $ f b

instance Applicative (Either' a) where
    pure = Right'
    (<*>) _ (Left' a) = Left' a
    (<*>) (Right' b) r = fmap b r

instance Foldable (Either' a) where
    foldMap _ (Left' _) = mempty
    foldMap f (Right' b) = f b
    foldr _ z (Left' a) = z
    foldr f x (Right' y) = f y x

instance Traversable (Either' a) where
    traverse _ (Left' x) = pure (Left' x)
    traverse f (Right' y) = Right' <$> f y

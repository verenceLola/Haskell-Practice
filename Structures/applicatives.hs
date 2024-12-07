module Web.Shipping.Utils ((<||>)) where

import           Control.Applicative (liftA2)

(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) = liftA2 (||)

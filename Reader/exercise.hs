module ReaderPractice where

import           Control.Applicative
import           Data.Maybe

x = [1, 2,3]
y = [4,5,6]
z = [7,8,9]

lookup :: Eq a => a -> [(a, b)] -> Maybe b

xs :: Maybe Integer
xs = undefined
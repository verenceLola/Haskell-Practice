module Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > (1 :: Integer) `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            2 + (2 :: Integer) `shouldBe` 4
        it "15 divided by 3 is 5" $ do
            divideby (15 :: Integer) 3 `shouldBe` (5, 0)
        it "22 divided by 5 is 4 remainder 2" $ do
            (22 :: Integer) `divideby` 5 `shouldBe` (4, 2)
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)

divideby :: Integral a => a -> a -> (a, a)
divideby num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n -d) d (count + 1)

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]


prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

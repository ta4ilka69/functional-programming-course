module PropertyBasedTest where

import AvlBag
import Data.List (sort)
import Test.HUnit
import Test.QuickCheck

instance (Ord a, Arbitrary a) => Arbitrary (AVLBag a) where
  arbitrary = do
    values <- arbitrary :: Gen [a]
    return (foldr insert Empty values)

prop_identity :: (Eq a, Ord a) => AVLBag a -> Bool
prop_identity bag = (bag <> mempty == bag) && (mempty <> bag == bag)

prop_associativity :: (Eq a, Ord a) => AVLBag a -> AVLBag a -> AVLBag a -> Bool
prop_associativity a b c = (a <> (b <> c)) == ((a <> b) <> c)

prop_foldConsistencyInt :: AVLBag Int -> Bool
prop_foldConsistencyInt bag =
  foldl (+) 0 bag == foldr (+) 0 bag

prop_foldConsistencyChar :: AVLBag Char -> Bool
prop_foldConsistencyChar bag =
  sort (foldl (++) [] (mapAVLBag (: []) bag)) == sort (foldr (++) [] (mapAVLBag (: []) bag))

prop_foldConsistencyString :: AVLBag String -> Bool
prop_foldConsistencyString bag =
  sort (foldl (++) [] bag) == sort (foldr (++) [] bag)

runPropertyTests :: IO ()
runPropertyTests = do
  putStrLn "Testing properties with Int..."
  quickCheck (prop_identity :: AVLBag Int -> Bool)
  quickCheck (prop_associativity :: AVLBag Int -> AVLBag Int -> AVLBag Int -> Bool)
  quickCheck prop_foldConsistencyInt

  putStrLn "Testing properties with Char..."
  quickCheck (prop_identity :: AVLBag Char -> Bool)
  quickCheck (prop_associativity :: AVLBag Char -> AVLBag Char -> AVLBag Char -> Bool)
  quickCheck prop_foldConsistencyChar

  putStrLn "Testing properties with String..."
  quickCheck (prop_identity :: AVLBag String -> Bool)
  quickCheck (prop_associativity :: AVLBag String -> AVLBag String -> AVLBag String -> Bool)
  quickCheck prop_foldConsistencyString

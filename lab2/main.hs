module Main where

import AvlBag
import Data.Monoid

main :: IO ()
main = do
  let bag = foldr insert Empty [5, 3, 8, 3, 10, 5, 5]
  putStrLn "Initial AVLBag:"
  print bag

  let bag2 = foldr insert Empty [1, 3, 6, 11, 21, -2, 6]
  let bagAfterDeletion = delete 3 bag
  putStrLn "\nAVLBag after deleting one occurrence of 3:"
  print bagAfterDeletion

  let bagAfterDeletions = delete 3 bagAfterDeletion
  putStrLn "\nAVLBag after deleting both occurrence of 3:"
  print bagAfterDeletions

  let bagAfterDeletionsAndInsertionBack = insert 3 bagAfterDeletions
  putStrLn "\nAVLBag after inserting back '3' value"
  print bagAfterDeletionsAndInsertionBack

  let mappedBag = mapAVLBag (* 2) bag
  putStrLn "\nAVLBag after mapping (*2):"
  print mappedBag

  let filteredBag = filterAVLBag even bag
  putStrLn "\nAVLBag after filtering even numbers:"
  print filteredBag

  let foldedSum = foldlAVLBag (+) 0 bag
  putStrLn "\nSum of elements in AVLBag:"
  print foldedSum

  let foldedProduct = foldrAVLBag (*) 1 bag
  putStrLn "\nProduct of elements in AVLBag:"
  print foldedProduct

  let emptyBag = (Empty :: AVLBag Int)
  let mergedBag = bag <> bagAfterDeletion
  putStrLn "\nMerged AVLBag:"
  print mergedBag

  putStrLn "\nMempty <> bag == bag:"
  print $ mempty <> bag == bag

  putStrLn "bag <> mempty == bag:"
  print $ bag <> mempty == bag

  let associativeBag1 = (bag <> bag2) <> mappedBag
  let associativeBag2 = bag <> (bag2 <> mappedBag)
  putStrLn "\na*(b*c) == (a*b)*c :"
  print $ associativeBag1 == associativeBag2

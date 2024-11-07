module UnitTest where

import AvlBag
import Data.Char (toUpper)
import Test.HUnit

testInsert :: Test
testInsert = TestCase $ do
  let bag = insert 10 (insert 5 (insert 5 Empty))
  assertEqual "Insert 5 twice and 10 once" [5, 5, 10] (toList bag)

testDelete :: Test
testDelete = TestCase $ do
  let bag = delete 5 (insert 10 (insert 5 (insert 5 Empty)))
  assertEqual "Delete one occurrence of 5" [5, 10] (toList bag)

testMap :: Test
testMap = TestCase $ do
  let bag = mapAVLBag (+ 1) (insert 3 (insert 2 (insert 1 Empty)))
  assertEqual "Map (+1) to all elements" [2, 3, 4] (toList bag)

testFilter :: Test
testFilter = TestCase $ do
  let bag = filterAVLBag (> 2) (insert 3 (insert 2 (insert 1 Empty)))
  assertEqual "Filter elements >2" [3] (toList bag)

testInsertChar :: Test
testInsertChar = TestCase $ do
  let bag = insert 'a' (insert 'c' (insert 'b' Empty))
  assertEqual "Insert 'a', 'b', 'c'" ['a', 'b', 'c'] (toList bag)

testDeleteString :: Test
testDeleteString = TestCase $ do
  let bag = delete "hello" (insert "world" (insert "hello" (insert "hello" Empty)))
  assertEqual "Delete one occurrence of 'hello'" ["hello", "world"] (toList bag)

testInsertDeleteChar :: Test
testInsertDeleteChar = TestCase $ do
  let bag = delete 'b' (insert 'c' (insert 'b' (insert 'a' (insert 'b' Empty))))
  assertEqual "Insert 'a', 'b' twice, 'c' and delete one 'b'" ['a', 'b', 'c'] (toList bag)

testMapChar :: Test
testMapChar = TestCase $ do
  let bag = mapAVLBag succ (insert 'a' (insert 'b' (insert 'c' Empty)))
  assertEqual "Map succ (next char) to all elements" ['b', 'c', 'd'] (toList bag)

testFilterChar :: Test
testFilterChar = TestCase $ do
  let bag = filterAVLBag (< 'c') (insert 'a' (insert 'b' (insert 'c' Empty)))
  assertEqual "Filter elements < 'c'" ['a', 'b'] (toList bag)

testInsertDeleteString :: Test
testInsertDeleteString = TestCase $ do
  let bag = delete "apple" (insert "banana" (insert "apple" (insert "apple" (insert "cherry" Empty))))
  assertEqual "Insert 'apple' twice, 'banana', 'cherry' and delete one 'apple'" ["apple", "banana", "cherry"] (toList bag)

testMapString :: Test
testMapString = TestCase $ do
  let bag = mapAVLBag (map toUpper) (insert "apple" (insert "banana" (insert "cherry" Empty)))
  assertEqual "Map toUpper to all elements" ["APPLE", "BANANA", "CHERRY"] (toList bag)

testFilterString :: Test
testFilterString = TestCase $ do
  let bag = filterAVLBag (\s -> length s > 5) (insert "apple" (insert "banana" (insert "cherry" Empty)))
  assertEqual "Filter elements with length > 5" ["banana", "cherry"] (toList bag)

unitTests :: Test
unitTests =
  TestList
    [ TestLabel "Insert Test" testInsert,
      TestLabel "Delete Test" testDelete,
      TestLabel "Map Test" testMap,
      TestLabel "Filter Test" testFilter,
      TestLabel "Insert Test with Char" testInsertChar,
      TestLabel "Delete Test with String" testDeleteString,
      TestLabel "Insert and Delete Test with Char" testInsertDeleteChar,
      TestLabel "Map Test with Char" testMapChar,
      TestLabel "Filter Test with Char" testFilterChar,
      TestLabel "Insert and Delete Test with String" testInsertDeleteString,
      TestLabel "Map Test with String" testMapString,
      TestLabel "Filter Test with String" testFilterString
    ]

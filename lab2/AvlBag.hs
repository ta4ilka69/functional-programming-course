{-# LANGUAGE DeriveFunctor #-}

module AvlBag where

import Data.List (sort)
import Data.Monoid

data AVLBag a
  = Empty
  | Node
      { left :: AVLBag a,
        value :: a,
        count :: Int,
        right :: AVLBag a,
        height' :: Int
      }
  deriving (Show, Functor)

height :: AVLBag a -> Int
height Empty = 0
height (Node _ _ _ _ h) = h

node :: AVLBag a -> a -> Int -> AVLBag a -> AVLBag a
node left value count right =
  Node left value count right (1 + max (height left) (height right))

balanceFactor :: AVLBag a -> Int
balanceFactor Empty = 0
balanceFactor (Node left _ _ right _) = height left - height right

rotateRight :: AVLBag a -> AVLBag a
rotateRight (Node (Node ll lv lc lr lh) v c r _) =
  node ll lv lc (node lr v c r)
rotateRight tree = tree

rotateLeft :: AVLBag a -> AVLBag a
rotateLeft (Node l v c (Node rl rv rc rr rh) _) =
  node (node l v c rl) rv rc rr
rotateLeft tree = tree

balance :: AVLBag a -> AVLBag a
balance t@(Node l v c r h)
  | bf > 1 =
      if balanceFactor l >= 0
        then rotateRight t
        else rotateRight (node (rotateLeft l) v c r)
  | bf < -1 =
      if balanceFactor r <= 0
        then rotateLeft t
        else rotateLeft (node l v c (rotateRight r))
  | otherwise = t
  where
    bf = balanceFactor t
balance t = t

insert :: (Ord a) => a -> AVLBag a -> AVLBag a
insert x Empty = Node Empty x 1 Empty 1
insert x (Node l v c r h)
  | x == v = Node l v (c + 1) r h
  | x < v = balance $ node (insert x l) v c r
  | otherwise = balance $ node l v c (insert x r)

findMin :: AVLBag a -> a
findMin (Node Empty v _ _ _) = v
findMin (Node l _ _ _ _) = findMin l
findMin Empty = error "Empty AVLBag"

removeMin :: AVLBag a -> AVLBag a
removeMin (Node Empty _ _ r _) = r
removeMin (Node l v c r h) = balance $ node (removeMin l) v c r
removeMin Empty = Empty

delete :: (Ord a) => a -> AVLBag a -> AVLBag a
delete _ Empty = Empty
delete x (Node l v c r h)
  | x < v = balance $ node (delete x l) v c r
  | x > v = balance $ node l v c (delete x r)
  | otherwise =
      if c > 1
        then Node l v (c - 1) r h
        else case (l, r) of
          (Empty, _) -> r
          (_, Empty) -> l
          _ ->
            let minRight = findMin r
             in balance $ node l minRight 1 (removeMin r)

mapAVLBag :: (Ord b) => (a -> b) -> AVLBag a -> AVLBag b
mapAVLBag _ Empty = Empty
mapAVLBag f (Node l v c r _) =
  let newValue = f v
      leftMapped = mapAVLBag f l
      rightMapped = mapAVLBag f r
   in balance $ node leftMapped newValue c rightMapped

filterAVLBag :: (Ord a) => (a -> Bool) -> AVLBag a -> AVLBag a
filterAVLBag _ Empty = Empty
filterAVLBag predicate (Node l v c r _) =
  let filteredLeft = filterAVLBag predicate l
      filteredRight = filterAVLBag predicate r
      current = if predicate v then replicateAVL c v else Empty
   in filteredLeft <> current <> filteredRight

replicateAVL :: (Ord a) => Int -> a -> AVLBag a
replicateAVL n x = foldr insert Empty (replicate n x)

foldlAVLBag :: (b -> a -> b) -> b -> AVLBag a -> b
foldlAVLBag _ acc Empty = acc
foldlAVLBag f acc (Node l v c r _) =
  let accAfterLeft = foldlAVLBag f acc l
      accAfterValue = foldl f accAfterLeft (replicate c v)
   in foldlAVLBag f accAfterValue r

foldrAVLBag :: (a -> b -> b) -> b -> AVLBag a -> b
foldrAVLBag _ acc Empty = acc
foldrAVLBag f acc (Node l v c r _) =
  let accAfterRight = foldrAVLBag f acc r
      accAfterValue = foldr f accAfterRight (replicate c v)
   in foldrAVLBag f accAfterValue l

instance (Ord a) => Semigroup (AVLBag a) where
  (<>) = union

instance (Ord a) => Monoid (AVLBag a) where
  mempty = Empty
  mappend = (<>)

union :: (Ord a) => AVLBag a -> AVLBag a -> AVLBag a
union Empty t = t
union t Empty = t
union (Node l1 v1 c1 r1 _) t2 =
  union l1 (r1 `union` insertMultiple v1 c1 t2)

insertMultiple :: (Ord a) => a -> Int -> AVLBag a -> AVLBag a
insertMultiple x n tree = foldr (const (insert x)) tree [1 .. n]

leftFold :: (b -> a -> b) -> b -> AVLBag a -> b
leftFold = foldlAVLBag

toList :: AVLBag a -> [a]
toList Empty = []
toList (Node l v c r _) = toList l ++ replicate c v ++ toList r

instance (Ord a) => Eq (AVLBag a) where
  bag1 == bag2 = sort (toList bag1) == sort (toList bag2)

rightFold :: (a -> b -> b) -> b -> AVLBag a -> b
rightFold = foldrAVLBag

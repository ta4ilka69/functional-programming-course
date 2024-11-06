module AvlBag where

data AvlBag a
  = Void
  | AvlNode
      { left :: AvlBag a,
        right :: AvlBag a,
        key :: a,
        count :: Integer,
        height :: Integer
      }
  deriving (Show)

AvlBag = Void
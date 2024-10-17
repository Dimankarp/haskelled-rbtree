-- For RBTree data
{-# OPTIONS_GHC -Wno-partial-fields #-}

module RBTree (RBTree.lookup, dict) where

data Color = Black | Red deriving (Show, Eq, Ord)

data (Ord a) => RBDictionary a b
  = Leaf
  | Node
      { color :: Color,
        key :: a,
        val :: b,
        left :: RBDictionary a b,
        right :: RBDictionary a b
      }
  deriving (Show, Eq)

type RBD = RBDictionary

lookup :: (Ord a) => a -> RBD a b -> Maybe b
lookup _ Leaf = Nothing
lookup k Node {key = nk, val = nv, left = nl, right = nr}
  | k == nk = Just nv
  | k < nk = RBTree.lookup k nl
  | otherwise = RBTree.lookup k nr

dict = Node {color = Red, key = 5, val = 6, left = Node {color = Red, key = 4, val = 7}, right = Leaf}

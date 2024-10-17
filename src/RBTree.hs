-- For RBTree data
{-# OPTIONS_GHC -Wno-partial-fields #-}

module RBTree (dict) where

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

lookup' :: (Ord a) => a -> RBD a b -> Maybe b
lookup' _ Leaf = Nothing
lookup' k Node {key = nk, val = nv, left = nl, right = nr}
  | k == nk = Just nv
  | k < nk = lookup' k nl
  | otherwise = lookup' k nr

insert' :: (Ord a) => a -> b -> RBD a b -> RBD a b

-- Root Insert
insert' k v Leaf = Node Black k v Leaf Leaf

--2-Node Insert
insert' k v n@Node {left = Leaf, right = Leaf, key = nk}
  | nk == k = n {val = v}
  | otherwise =
      if k < nk
        then
          n
            { left = Node Red k v Leaf Leaf
            }
        else
          n
            { right = Node Red k v Leaf Leaf
            }

-- 3-Node right child Insert
insert' k v n@Node {left = Leaf, right = nr@Node{}} 
  | nk == k = n {val = v}
  | k < nk = n {left = Node Red k v Leaf Leaf}
  | otherwise = 

insert' k v n@Node {color = nc, key = nk, left = nl, right = nr}
  | nk == k = n {val = v}
  | nc == Red = insertSkipNode
  | color nl == Red && color nr == Red =
      if k < nk
        then
          n {color = Red, left = insert' k v nl {color = Black}}
        else n {color = Red, right = insert' k v nr {color = Black}}
  | otherwise = Lea
  where
    insertSkipNode =
      if k < nk
        then
          n {left = insert' k v nl}
        else n {left = insert' k v nr}

dict = Node {color = Red, key = 5, val = 6, left = Node {color = Red, key = 4, val = 7}, right = Leaf}

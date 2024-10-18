-- For RBTree data
{-# OPTIONS_GHC -Wno-partial-fields #-}

module RBTree (dict, lookup', insert', fromList') where

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

{-

FromList implementation
  I wonder, if it's lazy (but looks like it's not)

-}
fromList' :: (Ord a) => [(a, b)] -> RBD a b
fromList' ls = go ls Leaf
  where
    go [] d = d
    go (x : xs) d = go xs $ uncurry insert' x d

{-

Lookup implementation
  basic binary tree search

-}
lookup' :: (Ord a) => a -> RBD a b -> Maybe b
lookup' _ Leaf = Nothing
lookup' k Node {key = nk, val = nv, left = nl, right = nr}
  | k == nk = Just nv
  | k < nk = lookup' k nl
  | otherwise = lookup' k nr

{-

Insert implementation
  insert' just wraps insertImpl' and maintains root as Black

-}
insert' :: (Ord a) => a -> b -> RBD a b -> RBD a b
insertImpl' :: (Ord a) => a -> b -> RBD a b -> RBD a b
insert' k v d = if color n == Black then n else n {color = Black}
  where
    n = insertImpl' k v d

-- Empty Insert
insertImpl' k v Leaf = Node Black k v Leaf Leaf
-- Red Skip Insert
insertImpl' k v n@Node {color = Red, key = nk, left = nl, right = nr}
  | nk == k = n {val = v}
  | k < nk = n {left = insertImpl' k v nl}
  | otherwise = n {right = insertImpl' k v nr}
-- 2-Node Insert
insertImpl' k v n@Node {color = Black, left = Leaf, right = Leaf, key = nk}
  | nk == k = n {val = v}
  | otherwise =
      if k < nk then n {left = Node Red k v Leaf Leaf} else n {right = Node Red k v Leaf Leaf}
-- 3-Node right child Insert
insertImpl' k v n@Node {key = nk, left = Leaf, right = nr@Node {key = nrk, left = nrl}}
  | k == nk = n {val = v}
  | k < nk = n {left = Node Red k v Leaf Leaf}
  | k == nrk = n {right = nr {val = v}}
  | k > nrk = nr {color = Black, left = n {color = Red, right = nrl}, right = Node Red k v Leaf Leaf}
  | otherwise = Node Black k v n {color = Red, right = Leaf} nr
-- 3-Node left child Insert
insertImpl' k v n@Node {key = nk, left = nl@Node {key = nlk, right = nlr}, right = Leaf}
  | k == nk = n {val = v}
  | k > nk = n {right = Node Red k v Leaf Leaf}
  | k == nlk = n {left = nl {val = v}}
  | k < nlk = nl {color = Black, right = n {color = Red, left = nlr}, left = Node Red k v Leaf Leaf}
  | otherwise = Node Black k v nl n {color = Red, left = Leaf}
-- 4-Node split Insert
insertImpl' k v n@Node {key = nk, left = nl@Node {color = Red}, right = nr@Node {color = Red}}
  | k == nk = n {val = v}
  | k < nk = n {color = Red, left = insertImpl' k v nl {color = Black}, right = nr {color = Black}}
  | otherwise = n {color = Red, left = nl {color = Black}, right = insertImpl' k v nr {color = Black}}
-- Regular move Insert
insertImpl' k v n@Node {key = nk, left = nl@Node {}, right = nr@Node {}}
  | k == nk = n {val = v}
  | k < nk = n {left = insertImpl' k v nl}
  | otherwise = n {right = insertImpl' k v nr}



dict = Node {color = Red, key = 5, val = 6, left = Node {color = Red, key = 4, val = 7, left = Leaf, right = Leaf}, right = Leaf}

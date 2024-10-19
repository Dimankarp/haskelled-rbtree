-- For RBTree data
{-# OPTIONS_GHC -Wno-partial-fields #-}

module RBTree (lookup', insert', fromList') where

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

remove' :: (Ord a) => a -> RBD a b -> RBD a b
remove' _ Leaf = Leaf
-- Root removal
remove' k n@Node {key = nk, left = Leaf, right = Leaf}
  | k == nk = Leaf
  | otherwise = n
remove' k n@Node {key = nk, left = Leaf, right = nr@Node {}}
  | k == nk = tryRemoveAndBalance' k n
  | otherwise = n {right = tryRemoveAndBalance' k nr}
remove' k n@Node {key = nk, left = nl@Node {}, right = Leaf}
  | k == nk = tryRemoveAndBalance' k n
  | otherwise = n {left = tryRemoveAndBalance' k nl}
remove' k n@Node {key = nk, left = nl@Node {}, right = nr@Node {}}
  | k == nk = n {key = key next, right = remove' (key next) n}
  | is2Node nl && is2Node nr = removeImpl' k n {left = nl {color = Red}, right = nr {color = Red}}
  | otherwise = removeImpl' k n
  where
    next = min' nr

removeImpl' :: (Ord a) => a -> RBD a b -> RBD a b
removeImpl' _ Leaf = Leaf

removeImpl' k n@Node {key = nk, color = Black, left = Leaf, right = nr@Node{}}
  | k < nk = n
  | otherwise = n{right = tryRemoveAndBalance' k nr}

removeImpl' k n@Node {key = nk, color = Black, left = nl@Node{}, right = Leaf}
  | k > nk = n
  | otherwise = n{left = tryRemoveAndBalance' k nl}

removeImpl' k n@Node {key = nk, color = Black, left = Leaf, right = Leaf}
  | k == nk = Leaf
  | otherwise = n
  
removeImpl' k n@Node {key = nk, color = Black, left = nl@Node{}, right = nr@Node{}}
  = if k < nk then removeLeft else removeRight
    where removeLeft = case nl of 
                    l@Node{color=Red, key = lk} -> if lk == k 
                      then 
                       case (left l, right l) of
                            (Leaf, _) -> n {left = tryRemoveAndBalance' k l}
                            (_, Leaf) -> n {left = tryRemoveAndBalance' k l} 
                            (_, _) -> removeImpl' k n {left=nl{key = key next, right = setMin' k $ right nl}}
                              where next = min' & right l 
                      else case (left l, right l) of
                         (ll@Node{}, lr@Node{}) -> 
removeImpl' _ _ = undefined
-- removeImpl' k n@Node{key = nk, color = Black, }

tryRemoveAndBalance' :: (Ord a) => a -> RBD a b -> RBD a b
tryRemoveAndBalance' _ Leaf = Leaf
tryRemoveAndBalance' k n@Node {color = Red, key = nk}
  | k /= nk = n
  | otherwise = Leaf
tryRemoveAndBalance' k n@Node {color = Black, key = nk, left = Leaf}
  | k /= nk = n
  | otherwise = if right n == Leaf then Leaf else (right n) {color = Black}
tryRemoveAndBalance' k n@Node {color = Black, key = nk, right = Leaf}
  | k /= nk = n
  | otherwise = (left n) {color = Black}
tryRemoveAndBalance' _ n@Node {color = Black} = n

min' :: (Ord a) => RBD a b -> RBD a b
min' Leaf = Leaf
min' n@Node {left = nl}
  | nl == Leaf = n
  | otherwise = min' nl

setMin' _ Leaf=  Leaf
setMin' k n@Node {left = nl}
  | nl == Leaf = n{key = k}
  | otherwise = setMin' k nl

is2Node n@Node {color = Black, left = Node {color = Black}, right = Node {color = Black}} = True
is2Node _ = False

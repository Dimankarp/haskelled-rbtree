{-# LANGUAGE InstanceSigs #-}
-- For RBTree data
{-# OPTIONS_GHC -Wno-partial-fields #-}

module RBTree (lookup', insert', fromList', remove', foldr'', foldl'', map', filter') where

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

instance (Ord a) => Semigroup (RBDictionary a b) where
  (<>) :: RBDictionary a b -> RBDictionary a b -> RBDictionary a b
  (<>) = foldr'' (\(k, v) acc -> insert' k v acc)

instance (Ord a) => Monoid (RBDictionary a b) where
  mempty :: RBDictionary a b
  mempty = Leaf

  mconcat :: [RBDictionary a b] -> RBDictionary a b
  mconcat dicts = go dicts Leaf
    where
      go [] n = n
      go (d : ds) n = go ds (n <> d)

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
remove' k d = fixRoot $ removeImpl' k d
  where
    fixRoot n@Node {} = n {color = Black}
    fixRoot Leaf = Leaf

removeImpl' :: (Ord a) => a -> RBD a b -> RBD a b
removeImpl' _ Leaf = Leaf
removeImpl' k n@Node {key = nk, left = nl, right = nr}
  | k < nk = removeLeft k n
  | k > nk = removeRight k n
  | otherwise = combine nl nr

removeLeft :: (Ord a) => a -> RBD a b -> RBD a b
removeLeft k n@Node {color = Black, left = nl} = balanceLeft $ n {left = removeImpl' k nl}
removeLeft k n@Node {color = Red, left = nl} = n {left = removeImpl' k nl}
removeLeft _ n = n

balanceLeft :: (Ord a) => RBD a b -> RBD a b
balanceLeft n@Node {color = Black, left = nl@Node {color = Red}} = n {color = Red, left = nl {color = Black}}
balanceLeft n@Node {color = Black, right = nr@Node {color = Black}} = balanceTree' n {right = nr {color = Red}}
balanceLeft n@Node {color = Black, right = nr@Node {color = Red, left = nrl@Node {color = Black}, right = nrr@Node {color = Black}}} =
  nrl {color = Red, left = n {right = left nrl}, right = balanceTree' $ nr {color = Black, left = right nrl, right = nrr {color = Red}}}
balanceLeft n = n

removeRight :: (Ord a) => a -> RBD a b -> RBD a b
removeRight k n@Node {color = Black, right = nr} = balanceRight $ n {right = removeImpl' k nr}
removeRight k n@Node {color = Red, right = nr} = n {right = removeImpl' k nr}
removeRight _ n = n

balanceRight :: (Ord a) => RBD a b -> RBD a b
balanceRight n@Node {color = Black, right = nr@Node {color = Red}} = n {color = Red, right = nr {color = Black}}
balanceRight n@Node {color = Black, left = nl@Node {color = Black}} = balanceTree' n {left = nl {color = Red}}
balanceRight n@Node {color = Black, left = nl@Node {color = Red, left = nll@Node {color = Black}, right = nlr@Node {color = Black}}} =
  nlr {color = Red, right = n {left = right nlr}, left = balanceTree' $ nl {color = Black, left = nll {color = Red}, right = left nlr}}
balanceRight n = n

balanceTree' :: (Ord a) => RBD a b -> RBD a b
balanceTree' (Node Black zk zv (Node Red yk yv (Node Red xk xv a b) c) d) = Node Red yk yv (Node Black xk xv a b) (Node Black zk zv c d)
balanceTree' (Node Black zk zv (Node Red xk xv a (Node Red yk yv b c)) d) = Node Red yk yv (Node Black xk xv a b) (Node Black zk zv c d)
balanceTree' (Node Black xk xv a (Node Red zk zv (Node Red yk yv b c) d)) = Node Red yk yv (Node Black xk xv a b) (Node Black zk zv c d)
balanceTree' (Node Black xk xv a (Node Red yk yv b (Node Red zk zv c d))) = Node Red yk yv (Node Black xk xv a b) (Node Black zk zv c d)
balanceTree' n = n

combine :: (Ord a) => RBD a b -> RBD a b -> RBD a b
combine Leaf n = n
combine n Leaf = n
combine l@Node {color = Black} r@Node {color = Red} = r {left = combine l $ left r}
combine l@Node {color = Red} r@Node {color = Black} = l {right = combine (right l) r}
combine l@Node {color = Red} r@Node {color = Red} =
  let s = combine (right l) (left r)
   in case s of
        rs@Node {color = Red} -> rs {left = l {right = left rs}, right = r {left = right rs}}
        Node {color = Black} -> l {right = r {left = s}}
        _ -> undefined
combine l@Node {color = Black} r@Node {color = Black} =
  let s = combine (right l) (left r)
   in case s of
        rs@Node {color = Red} -> rs {left = l {right = left rs}, right = r {left = right rs}}
        Node {color = Black} -> balanceLeft l {right = r {left = s}}
        _ -> undefined

foldr'' :: (Ord a) => ((a, b) -> c -> c) -> c -> RBD a b -> c
foldr'' _ acc Leaf = acc
foldr'' f acc n@Node {} = foldr'' f rightAcc (left n)
  where
    rightAcc = f (key n, val n) $ foldr'' f acc (right n)

foldl'' :: (Ord a) => ((a, b) -> c -> c) -> c -> RBD a b -> c
foldl'' _ acc Leaf = acc
foldl'' f acc n@Node {} = foldl'' f (f (key n, val n) leftAcc) (right n)
  where
    leftAcc = foldl'' f acc (left n)

map' :: (Ord a) => (b -> c) -> RBD a b -> RBD a c
map' _ Leaf = Leaf
map' p n@Node {} = n {val = p $ val n, left = map' p $ left n, right = map' p $ right n}

filter' :: (Ord a) => (b -> Bool) -> RBD a b -> RBD a b
filter' p = foldr'' (\(k, v) d -> if p v then insert' k v d else d) (fromList' [])

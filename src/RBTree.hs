-- For RBDictionary data
{-# OPTIONS_GHC -Wno-partial-fields #-}

{-

Originally the implementation was based on my algorithms class
explanation (or, more correctly, on a mix of an insert from algo
class and removal from @sweirich - which itself is ased on Okasaki).

Since it look a bit out of order and really messy (mostly because
implementation from my algo classes really favored Top-down approach -
which, efficient and cool in imperative DS, is really hard to implement
in functional context.

I ended up using Okasaki's (Functional Data Structures paper) approach
to insertion and Matt Might's removal (https://matt.might.net/articles/red-black-delete/).

-}

module RBTree (lookup', insert', fromList', RBDictionary (..), foldr'', foldl'', map', filter', remove', isHeightvalid, isColorlyValid) where

-- Order is significant for redder & blacker
data Color
  = NB -- Negative Black (used in removal)
  | R -- Read
  | B -- Black
  | BB -- Double Black (used in removal)
  deriving (Show, Eq, Ord, Enum)

data (Ord a) => RBDictionary a b
  = Leaf
  | BBLeaf -- Double black leaf (used in removal)
  | Node
      { color :: Color,
        key :: a,
        val :: b,
        left :: RBDictionary a b,
        right :: RBDictionary a b
      }
  deriving (Show)

type RBD = RBDictionary

blacker :: (Ord a) => RBD a b -> RBD a b
blacker Node {color = BB} = error "can't go blacker than double black"
blacker n@Node {} = n {color = succ (color n)}
blacker BBLeaf = error "can't go blacker than double black"
blacker Leaf = BBLeaf

redder :: (Ord a) => RBD a b -> RBD a b
redder Node {color = NB} = error "can't go redder than negative black"
redder n@Node {} = n {color = pred (color n)}
redder BBLeaf = Leaf
redder Leaf = error "can't go redder for a leaf"

isBB :: (Ord a) => RBD a b -> Bool
isBB Node {color = BB} = True
isBB BBLeaf = True
isBB _ = False

{-
=======================
Monoid implementation
=======================
-}
instance (Ord a) => Semigroup (RBDictionary a b) where
  (<>) = foldr'' (\(k, v) acc -> insert' k v acc)

instance (Ord a) => Monoid (RBDictionary a b) where
  mempty = Leaf
  mconcat dicts = go dicts Leaf
    where
      go [] n = n
      go (d : ds) n = go ds (n <> d)

{-

Should be more effective than folding into list
  and checking linearly because of LAZINESS.
  Obviously, still O(n) though.

-}
instance (Eq b, Ord a) => Eq (RBDictionary a b) where
  (==) a b =
    and $
      zipWith
        (\(k1, v1) (k2, v2) -> k1 == k2 && v1 == v2)
        (foldr'' join [] a)
        (foldr'' join [] b)
    where
      join (k, v) acc = (k, v) : acc

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
=======================
Lookup implementation
  basic binary tree search
=======================
-}
lookup' :: (Ord a) => a -> RBD a b -> Maybe b
lookup' _ Leaf = Nothing
lookup' k Node {key = nk, val = nv, left = nl, right = nr}
  | k == nk = Just nv
  | k < nk = lookup' k nl
  | otherwise = lookup' k nr
lookup' _ BBLeaf = error "double black leaf in lookup context"

{-
=======================
Insert implementation
  insert' just wraps insertImpl' and maintains root as Black
=======================
-}
insert' :: (Ord a) => a -> b -> RBD a b -> RBD a b
insert' k v d = new {color = B}
  where
    new = insertImpl' k v d

insertImpl' :: (Ord a) => a -> b -> RBD a b -> RBD a b
insertImpl' k v Leaf = Node R k v Leaf Leaf
insertImpl' k v n@Node {key = nk, left = nl, right = nr}
  | k < nk = balance (n {left = insertImpl' k v nl})
  | k == nk = n {val = v}
  | k > nk = balance (n {right = insertImpl' k v nr})
  | otherwise = error "unreachable"
insertImpl' _ _ BBLeaf = error "double black leaf in lookup context"

{-
=======================
Remove implementation
  for explanation check header of this file
=======================
-}
remove' :: (Ord a) => a -> RBD a b -> RBD a b
remove' k n@Node {} = new
  where
    new = case removeImpl' k n of
      node@Node {} -> node {color = B}
      Leaf -> Leaf
      BBLeaf -> Leaf
remove' _ n = n

removeImpl' :: (Ord a) => a -> RBD a b -> RBD a b
removeImpl' _ Leaf = Leaf
removeImpl' k n@Node {key = nk, left = nl, right = nr}
  | k < nk = bubble $ n {left = removeImpl' k nl}
  | k == nk = removeNode n
  | k > nk = bubble $ n {right = removeImpl' k nr}
  | otherwise = error "unreachable"
removeImpl' _ _ = error "removeImpl called for a leaf"

findMin :: (Ord a) => RBD a b -> RBD a b
findMin Leaf = Leaf
findMin n@Node {left = Leaf} = n
findMin Node {left = nl@Node {}} = findMin nl
findMin Node {left = BBLeaf} = error "double black leaf in findMin context"
findMin BBLeaf = error "double black leaf in findMin context"

removeMin :: (Ord a) => RBD a b -> RBD a b
removeMin n@Node {left = nl@Node {}} = bubble $ n {left = removeMin nl}
removeMin n@Node {left = Leaf} = removeNode n
removeMin Leaf = Leaf
removeMin _ = error "BBLeaf in removeMin context"

removeNode :: (Ord a) => RBD a b -> RBD a b
removeNode Node {color = R, left = Leaf, right = Leaf} = Leaf
removeNode Node {color = B, left = Leaf, right = Leaf} = BBLeaf
removeNode Node {color = B, left = Leaf, right = nr@Node {color = R}} = nr {color = B}
removeNode Node {color = B, left = nl@Node {color = R}, right = Leaf} = nl {color = B}
removeNode n@Node {left = Node {}, right = nr@Node {}} = bubble $ n {val = val minNode, key = key minNode, right = removeMin nr}
  where
    minNode = findMin nr
removeNode _ = error "all expected matchings failed, either invatiant is broken or BBLeaf in removeNode context"

{-
 Passes double black nodes up
  the tree
-}
bubble :: (Ord a) => RBD a b -> RBD a b
bubble n@Node {left = nl, right = nr}
  | isBB nl || isBB nr = balance $ blacker n {left = redder nl, right = redder nr}
  | otherwise = n
bubble n = n

{-
 Fixes double red invariant violation
  record syntax is not used here,
  since it makes things really messy
-}
balance :: (Ord a) => RBD a b -> RBD a b
-- Okasaki classic with Black Nodes
balance (Node B z zv (Node R x xv a (Node R y yv b c)) d) = Node R y yv (Node B x xv a b) (Node B z zv c d)
balance (Node B z zv (Node R y yv (Node R x xv a b) c) d) = Node R y yv (Node B x xv a b) (Node B z zv c d)
balance (Node B x xv a (Node R y yv b (Node R z zv c d))) = Node R y yv (Node B x xv a b) (Node B z zv c d)
balance (Node B x xv a (Node R z zv (Node R y yv b c) d)) = Node R y yv (Node B x xv a b) (Node B z zv c d)
-- Matt Might Double Black Nodes reduction
balance (Node BB z zv (Node R x xv a (Node R y yv b c)) d) = Node B y yv (Node B x xv a b) (Node B z zv c d)
balance (Node BB z zv (Node R y yv (Node R x xv a b) c) d) = Node B y yv (Node B x xv a b) (Node B z zv c d)
balance (Node BB x xv a (Node R y yv b (Node R z zv c d))) = Node B y yv (Node B x xv a b) (Node B z zv c d)
balance (Node BB x xv a (Node R z zv (Node R y yv b c) d)) = Node B y yv (Node B x xv a b) (Node B z zv c d)
-- Matt Might Negative Black Nodes reduction
balance (Node BB z zv (Node NB x xv (Node B w wv a b) (Node B y yv c d)) e) = Node B y yv (balance $ Node B x xv (Node R w wv a b) c) (Node B z zv d e)
balance (Node BB z zv a (Node NB x xv (Node B w wv b c) (Node B y yv d e))) = Node B w wv (Node B z zv a b) (balance $ Node B x xv c (Node R y yv d e))
balance n = n

{-
=======================
Utilities implementation
=======================
-}
foldr'' :: (Ord a) => ((a, b) -> c -> c) -> c -> RBD a b -> c
foldr'' _ acc Leaf = acc
foldr'' f acc n@Node {} = foldr'' f rightAcc (left n)
  where
    rightAcc = f (key n, val n) $ foldr'' f acc (right n)
foldr'' _ _ BBLeaf = error "Double Black leaf in foldr context"

foldl'' :: (Ord a) => ((a, b) -> c -> c) -> c -> RBD a b -> c
foldl'' _ acc Leaf = acc
foldl'' f acc n@Node {} = foldl'' f (f (key n, val n) leftAcc) (right n)
  where
    leftAcc = foldl'' f acc (left n)
foldl'' _ _ BBLeaf = error "Double Black leaf in foldl context"

map' :: (Ord a) => (b -> c) -> RBD a b -> RBD a c
map' _ Leaf = Leaf
map' p n@Node {} = n {val = p $ val n, left = map' p $ left n, right = map' p $ right n}
map' _ BBLeaf = error "Double Black leaf in map context"

filter' :: (Ord a) => (b -> Bool) -> RBD a b -> RBD a b
filter' p = foldr'' (\(k, v) d -> if p v then insert' k v d else d) (fromList' [])

instance (Ord a) => Functor (RBDictionary a) where
  fmap = map'

instance (Ord a) => Foldable (RBDictionary a) where
  foldr _ acc Leaf = acc
  foldr f acc n@Node {} = foldr f rightAcc (left n)
    where
      rightAcc = f (val n) $ foldr f acc (right n)
  foldr _ _ BBLeaf = error "Double Black leaf in foldr context"

{-
=======================
Invariant Check Implementation
  used by QuickCheck
=======================
-}

isColorlyValid :: (Ord a) => RBD a b -> Bool
isColorlyValid Leaf = True
isColorlyValid d@Node {color = B} = go d
  where
    go Leaf = True
    go Node {color = B, left = nl, right = nr} = go nr && go nl
    go Node {color = R, left = nl@Node {color = B}, right = nr@Node {color = B}} = go nr && go nl
    go Node {color = R, left = nl@Node {color = B}, right = Leaf} = go nl
    go Node {color = R, left = Leaf, right = nr@Node {color = B}} = go nr
    go Node {color = R, left = Leaf, right = Leaf} = True
    go _ = False
isColorlyValid _ = False

isHeightvalid :: (Ord a) => RBD a b -> Bool
isHeightvalid Leaf = True
isHeightvalid d@Node {} = snd $ go d
  where
    go Leaf = (1 :: Integer, True)
    go Node {color = col, left = nl, right = nr} =
      ( fst l
          + if col == B
            then 1
            else 0,
        snd l && snd r && (fst l == fst r)
      )
      where
        l = go nl
        r = go nr
    go _ = (0, False)
isHeightvalid _ = False

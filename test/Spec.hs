import Data.Maybe (isJust, isNothing)
import RBTree
import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = do
  -- quickCheck (monoidAssoc :: (RBDictionary Int Int) -> (RBDictionary Int Int) -> (RBDictionary Int Int) -> Bool)
  -- quickCheck (monoidLeftIdentity :: (RBDictionary Int Int) -> Bool)
  -- quickCheck (monoidRightIdentity :: (RBDictionary Int Int) -> Bool)
  runTestTTAndExit tests

equalityTest :: Test
equalityTest = TestCase (assertEqual "Simple Equality" a b)
  where
    a = fromList' [(1 :: Int, 2 :: Int), (4, 4), (6, 8), (9, 10)]
    b = fromList' [(9, 10), (4, 4), (6, 8), (1, 2)]

inequalityTest :: Test
inequalityTest = TestCase (assertBool "Simple Ineqaulity" (a /= b))
  where
    a = fromList' [(1 :: Int, 2 :: Int), (4, 4), (6, 8), (9, 10)]
    b = fromList' [(9, 10), (4, 4), (6, 8)]

checkLookup :: (Ord a, Show a) => RBDictionary a b -> (a, b) -> Assertion
checkLookup d (k, v) = do
  assertBool ("Key " ++ show k ++ " should now be in dict ") $ isJust $ lookup' k nd
  where
    nd = insert' k v d

checkInsert :: Test
checkInsert =
  TestCase
    ( do
        let list = [(x, x) | x <- [1 :: Int .. 1001]]
            d = fromList' list
        mapM_ (checkLookup d) list
    )

checkInsertFull :: Test
checkInsertFull =
  TestCase (assertBool "All elements must be in dictionary" $ and ls)
    where list = [(x, x) | x <- [1 .. 1000 :: Int]]
          d  = fromList' list
          ls = [fst v == Just (snd v) | v <- zip (map (flip lookup' d . fst) list) (map snd list)]

checkRemoval :: (Ord a, Show a) => RBDictionary a b -> a -> Assertion
checkRemoval d k = do
  assertBool ("Key " ++ show k ++ " should not be in dict ") $ isNothing $ lookup' k nd
  where
    nd = remove' k d

conseqRemoval :: Test
conseqRemoval =
  TestCase
    ( do
        let list = [(x, x) | x <- [1 :: Int .. 1001]]
            d = fromList' list
        mapM_ (checkRemoval d . fst) list
    )

checkMap :: Test
checkMap = TestCase (assertEqual "Mapping to squares" a b)
  where
    a = map' (\v -> v * v) $ fromList' [(x, x) | x <- [1 .. 1000 :: Int]]
    b = fromList' [(x, x * x) | x <- [1 .. 1000 :: Int]]

checkFold :: Test
checkFold = TestCase (assertEqual "Folding to sum" a b)
  where
    a = foldr'' (\(_, v) acc -> v + acc) (0 :: Int) $ fromList' [(x, x) | x <- [1 .. 1000 :: Int]]
    b = sum [1 .. 1000 :: Int]

tests :: Test
tests =
  TestList
    [ TestLabel "Equality check" equalityTest,
      TestLabel "Inequlity check" inequalityTest,
      TestLabel "Consequtive Insert" checkInsert,
      TestLabel  "Full lookup check after all inserts" checkInsertFull,
      TestLabel "Consequtive Removal" conseqRemoval,
      TestLabel "Mapping" checkMap,
      TestLabel "Folding to sum" checkFold
    ]

-- instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (RBDictionary a b) where
--   arbitrary = do
--     pairs <- listOf ((,) <$> arbitrary <*> arbitrary)
--     return $ fromList' pairs

-- monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
-- monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
-- monoidLeftIdentity a = (mempty <> a) == a

-- monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
-- monoidRightIdentity a = (a <> mempty) == a

-- countBlackHeight :: (Ord (a)) => RBDictionary a b -> Int -> Maybe Int
-- countBlackHeight Leaf n = Just n + 1
-- countBlackHeight d@Node{}  n = if lHeight == rHeight then Just lHeight else Nothing
--   where
--     addition = if color d == Black then 1 else 0
--     lHeight = countBlackHeight (left d) $ n + 1
--     rHeight = countBlackHeight (right d) $ n + 1
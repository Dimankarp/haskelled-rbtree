import Data.Maybe (isNothing)
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
    a =  fromList' [(1,2), (4, 4), (6,8), (9, 10)]
    b = fromList' [(9,10), (4, 4), (6,8), (1, 2)]
 
-- inequalityTest :: Test
-- inequalityTest = TestCase (assertBool "Simple Ineqaulity" (a==b))
--   where
--     a =  fromList' [(1,2), (4, 4), (6,8), (9, 10)]
--     b = fromList' [(9,10), (4, 4), (6,8)]

conseqInsert :: Test
conseqInsert = TestCase (assertEqual "Conseq new insert" a b)
  where
    a = insert' 1001 1001 (fromList' [(x, x) | x <- [1 .. 1000 :: Integer]])
    b = fromList' [(x, x) | x <- [1 .. 1001]]

conseqRemoval :: Test
conseqRemoval = TestCase (assertEqual "Conseq removal" a b)
  where
    a = insert' 1001 1001 (fromList' [(x, x) | x <- [1 .. 1000 :: Integer]])
    b = fromList' [(x, x) | x <- [1 .. 1001]]


checkFail :: Test
checkFail = TestCase (assertEqual "Conseq new insert" a True)
  where
    a = isNothing (lookup' 1001 (fromList' [(x, x) | x <- [1 .. 1000 :: Integer]]))

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
    [ TestLabel "Consequtive Insert" conseqInsert,
      TestLabel "Check for non existent value" checkFail,
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
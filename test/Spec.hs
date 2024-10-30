{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}

import Data.Maybe (isJust, isNothing)
import RBTree
import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = do
  print "monoidAssoc"
  quickCheck (monoidAssoc :: RBDictionary Int Int -> RBDictionary Int Int -> RBDictionary Int Int -> Bool)
  print "monoidLeftIdentity"
  quickCheck (monoidLeftIdentity :: RBDictionary Int Int -> Bool)
  print "monoidRightIdentity"
  quickCheck (monoidRightIdentity :: RBDictionary Int Int -> Bool)
  print "redblackColorInvariant"
  quickCheck (redblackColorInvariant :: RBDictionary Int Int -> Bool)
  print "redblackHeightInvariant"
  quickCheck (redblackHeightInvariant :: RBDictionary Int Int -> Bool)
  print "redblackFullInvariant"
  quickCheck (redblackFullInvariant :: RBDictionary Int Int -> Bool)
  print "redblackColorInvariantAfterActions"
  quickCheck (redblackColorInvariantAfterActions :: RBDictionary Int Int -> [Int] -> [(Int, Int)] -> Bool)
  print "redblackHeightInvariantAfterActions"
  quickCheck (redblackHeightInvariantAfterActions :: RBDictionary Int Int -> [Int] -> [(Int, Int)] -> Bool)
  print "redblackFullInvariantAfterActions"
  quickCheck (redblackFullInvariantAfterActions :: RBDictionary Int Int -> [Int] -> [(Int, Int)] -> Bool)

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
  where
    list = [(x, x) | x <- [1 .. 1000 :: Int]]
    d = fromList' list
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
      TestLabel "Full lookup check after all inserts" checkInsertFull,
      TestLabel "Consequtive Removal" conseqRemoval,
      TestLabel "Mapping" checkMap,
      TestLabel "Folding to sum" checkFold
    ]

instance (Ord a, Eq a, Arbitrary a, Arbitrary b) => Arbitrary (RBDictionary a b) where
  arbitrary = do
    pairs <- listOf ((,) <$> arbitrary <*> arbitrary)
    return $ fromList' pairs

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

redblackColorInvariant :: (Ord a) => RBDictionary a b -> Bool
redblackColorInvariant = isColorlyValid

redblackHeightInvariant :: (Ord a) => RBDictionary a b -> Bool
redblackHeightInvariant = isHeightvalid

redblackFullInvariant :: (Ord a) => RBDictionary a b -> Bool
redblackFullInvariant d = isColorlyValid d && isHeightvalid d

redblackColorInvariantAfterActions :: (Ord a) => RBDictionary a b -> [a] -> [(a, b)] -> Bool
redblackColorInvariantAfterActions d rmls insls = isColorlyValid insd
  where
    rmd = gorm d rmls
      where
        gorm dict [] = dict
        gorm dict (x : xs) = gorm (remove' x dict) xs
    insd = goins rmd insls
      where
        goins dict [] = dict
        goins dict (x : xs) = goins (uncurry insert' x dict) xs

redblackHeightInvariantAfterActions :: (Ord a) => RBDictionary a b -> [a] -> [(a, b)] -> Bool
redblackHeightInvariantAfterActions d rmls insls = isHeightvalid insd
  where
    rmd = gorm d rmls
      where
        gorm dict [] = dict
        gorm dict (x : xs) = gorm (remove' x dict) xs
    insd = goins rmd insls
      where
        goins dict [] = dict
        goins dict (x : xs) = goins (uncurry insert' x dict) xs

redblackFullInvariantAfterActions :: (Ord a) => RBDictionary a b -> [a] -> [(a, b)] -> Bool
redblackFullInvariantAfterActions d rmls insls = isColorlyValid insd && isHeightvalid insd
  where
    rmd = gorm d rmls
      where
        gorm dict [] = dict
        gorm dict (x : xs) = gorm (remove' x dict) xs
    insd = goins rmd insls
      where
        goins dict [] = dict
        goins dict (x : xs) = goins (uncurry insert' x dict) xs

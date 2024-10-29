module Main (main) where

import RBTree as RB

main :: IO ()
main = print $ RB.remove' 2 $ RB.fromList' [(1 :: Integer, 1 :: Integer), (2, 2), (3, 3), (4, 4), (5, 5)]

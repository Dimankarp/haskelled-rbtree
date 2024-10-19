module Main (main) where

import RBTree as RB

main :: IO ()
main =  print $ RB.filter' (\v -> v > 3) $ RB.fromList' [(1,1), (2,2), (3,3), (4,4), (5,5)]

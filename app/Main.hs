module Main (main) where

import RBTree as RB

main :: IO ()
main =  print $ RB.lookup' 512 $ RB.fromList' [(x, x+1) | x <- [1..10000]] --print $ RB.insert' 1 12 RB.dict

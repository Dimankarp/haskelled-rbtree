module Main (main) where

import RBTree

main :: IO ()
main = putStrLn $ show $ RBTree.lookup 4 RBTree.dict

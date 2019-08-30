module Main where

import Task5_1

main :: IO ()
main = let d = list2dlist [42, 73] in putStrLn $ show $ index d 0

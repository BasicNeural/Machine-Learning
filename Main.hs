module Main where

import ML.GD
import ML.Regression.Linear
import Data.Matrix

main = do
    let x = fromLists [[1,1,1,1,1],[1,0,3,0,5],[0,2,0,4,0]]
    let w = fromLists [[0,0,0]]
    let y = fromLists [[1,2,3,4,5]]
    putStrLn . show $ gd linear w x y
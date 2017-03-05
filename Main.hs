module Main where

import ML.NN
import Data.Matrix

main = do
    x = map (transpose . fromLists . (:[])) [[0,0],[1,0],[0,1],[1,1]]
    y = map (fromLists . (:[])) [[0],[1],[1],[0]]

    let s1 = fromLists 2 3 $ replicate 6 0.1
    let s2 = fromLists 2 2 $ replicate 4 0.1
    let h1 = (Layer sigmoid s1)
    let h2 = (Layer sigmoid s2)
    let dataset = zip x y
    result -> sdg 0.01 2000 logistic [h1,h2]

    sequence result (putStrLn . show)
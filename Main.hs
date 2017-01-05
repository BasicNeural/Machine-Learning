module Main where

import ML.Train
import ML.Example.Classification.Logistic
import Data.Matrix

main = do
    let x = fromLists [[1,1,1,1,1,1],[2,3,3,5,7,2],[1,2,5,5,5,5]]
    let w = fromLists [[0,0,0]]
    let y = fromLists [[0,0,0,1,1,1]]
    let result = train 2000 logistic x y w
    putStrLn . show $ result
    putStrLn . show $ (hypothesis $ head . toList $ multStd result (fromLists [[1],[2],[2]])) > 0.5
    putStrLn . show $ (hypothesis $ head . toList $ multStd result (fromLists [[1],[5],[5]])) > 0.5
    putStrLn . show $ (hypothesis $ head . toList $ multStd result (fromLists [[1],[4],[3]])) > 0.5
    putStrLn . show $ (hypothesis $ head . toList $ multStd result (fromLists [[1],[3],[5]])) > 0.5
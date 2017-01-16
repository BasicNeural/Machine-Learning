module Main where

import ML.Train
import ML.Example.Classification.Logistic
import Data.Matrix

softmax x = scaleMatrix (1 / (sum . map exp $ toList x)) $ fmap exp x

main = do
    let x = fromLists [[1,1,1,1,1,1,1,1],[2,3,3,5,7,2,6,7],[1,2,4,5,5,5,6,7]]
    let w = fromLists [[0,0,0]]
    let y1 = transpose . fromLists $ [[0,0,0,0,0,0,1,1]]
    let y2 = transpose . fromLists $ [[0,0,0,1,1,1,0,0]]
    let y3 = transpose . fromLists $ [[1,1,1,0,0,0,0,0]]
    let r1 = trainMatrix 10000 logistic x y1 w
    let r2 = trainMatrix 10000 logistic x y2 w
    let r3 = trainMatrix 10000 logistic x y3 w
    let r = r1 <-> r2 <-> r3
    putStrLn "weights"
    putStrLn . show $ r

    let test1 = softmax $ multStd r (fromLists [[1],[3],[4]])

    putStrLn "softmax result weights"

    putStrLn . show $ test1

    putStr "your class is "
    putChar $ snd . maximum $ zip (toList test1) "ABC"
    putStrLn ""
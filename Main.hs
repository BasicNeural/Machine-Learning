module Main where

import ML.Train
import ML.Example.Classification.Softmax
import Data.Matrix

main = do
    let x = fromLists [[1,1,1,1,1,1,1,1],[2,3,3,5,7,2,6,7],[1,2,4,5,5,5,6,7]]
    let w = fromLists [[0,0,0],[0,0,0],[0,0,0]]
    let y = transpose . fromLists $ [[0,0,0,0,0,0,1,1],[0,0,0,1,1,1,0,0],[1,1,1,0,0,0,0,0]]

    let r = trainMatrix 0.001 4000 loss x y w

    putStrLn "weights"
    putStrLn . show $ r

    let test1 = softmax $ multStd (fromLists [[1,11,7]]) r
    let test2 = softmax $ multStd (fromLists [[1,5,5]]) r
    let test3 = softmax $ multStd (fromLists [[1,2,2]]) r
    

    putStrLn "softmax result weights"

    putStrLn . show $ test1

    putStr "your class is "
    putChar $ snd . maximum $ zip (toList test1) "ABC"
    putStrLn "\n"
    
    putStrLn "softmax result weights"

    putStrLn . show $ test2

    putStr "your class is "
    putChar $ snd . maximum $ zip (toList test2) "ABC"
    putStrLn "\n"
    
    putStrLn "softmax result weights"

    putStrLn . show $ test3
    
    putStr "your class is "
    putChar $ snd . maximum $ zip (toList test3) "ABC"
    putStrLn "\n"
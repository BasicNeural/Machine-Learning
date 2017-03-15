module Main where

import ML.NN
import Data.Matrix
import System.Random

makeRand x = replicate x $ randomRIO (-2.0, 2.0)

main = do
    let x = map (transpose . fromLists . (:[])) [[0,0],[1,0],[0,1],[1,1]]
    let y = map (fromLists . (:[])) [[0],[1],[1],[0]]
    seed1 <- sequence $ makeRand 4
    seed2 <- sequence $ makeRand 2
    let s1 = fromList 2 2 seed1
    let b1 = fromList 2 1 $ replicate 2 0
    let s2 = fromList 1 2 $ seed2
    let b2 = fromList 1 1 $ replicate 1 0
    let h1 = (Layer sigmoid s1 b1)
    let h2 = (Layer sigmoid s2 b2)
    let dataset = zip x y

    putStrLn "Start!"
    
    result <- sdg 0.1 20000 [h1,h2] dataset
    putStrLn "Result :"
    putStrLn . show $ result
    putStrLn "Test :"
    putStrLn . show $ map (executeLayer result) x
module Main where

import ML.NN
import Text.CSV
import System.Random
import Data.Matrix

makeRand x = replicate x $ randomRIO (-2.0, 2.0)

rank x = snd $ maximum r
    where r = zip (toList x) [1.. length x]

test x y w = do
    let result = executeLayer w x
    putStr "예측 등급 : "
    putStrLn . show $ rank result
    putStr "실제 등급 : "
    putStrLn . show $ rank y
    putStrLn ""

main = do
    (Right rawdata) <- parseCSVFromFile ".\\resource\\wine_rank.csv"

    let dataset = map (map (\x -> read x :: Double)) $ rawdata

    let x = map (transpose . fromLists . (:[]) . take 13) dataset
    let y = map (transpose . fromLists . (:[]) . drop 13) dataset

    seed1 <- sequence $ makeRand 52
    seed2 <- sequence $ makeRand 12
    let s1 = fromList 4 13 seed1
    let b1 = fromList 4 1 $ replicate 4 0
    let s2 = fromList 3 4 $ seed2
    let b2 = fromList 3 1 $ replicate 3 0
    let h1 = (Layer sigmoid s1 b1)
    let h2 = (Layer sigmoid s2 b2)
    let dataset = zip x y

    putStrLn "Start!"
    
    result <- sdg 0.1 20000 [h1,h2] dataset

    test (x !! 4) (y !! 4) result
    test (x !! 12) (y !! 12) result
    test (x !! 11) (y !! 11) result
    test (x !! 7) (y !! 7) result
    test (x !! 9) (y !! 9) result
    test (x !! 15) (y !! 15) result
    test (x !! 17) (y !! 17) result
    test (x !! 8) (y !! 8) result
module Main where

import ML.Train
import ML.Example.Regression.Linear
import Data.Matrix as M
import Data.List   as L
import Text.CSV

predict w x r = do
    let result = head . M.toList $ w * x
    putStr "입력값 : "
    putStrLn . show $ M.transpose x
    putStr "예측값 :"
    putStrLn $ show result
    putStr "실제값 :"
    putStrLn $ show r
    putStr "오차율 : "
    putStr . show $ abs (r - result) / r * 100
    putStrLn "%"
    putStrLn ""

main = do

    (Right rawdata) <- parseCSVFromFile "C:\\Users\\wjsgm\\Documents\\data.csv"

    let dataset = L.transpose . map (map (\x -> read x :: Double)) $ rawdata

    let x = fromLists $ (replicate (length . head $ dataset) 1) : init dataset
    let y = fromLists [last dataset]

    let w = fromLists [replicate (nrows x) 0]
    
    let result = trainMatrix 0.0001 20000 linear x y w

    putStrLn "학습 결과"
    putStrLn . show $ result
    
{-
키   무게  성공률 성공률(자유투) 평균득점
7.4  240   0.599    0.713       17.1
6.8  225   0.482    0.701       11.6
6.8  215   0.457    0.734       5.8
7    230   0.435    0.764       8.3
-}
    let d1 = M.transpose . fromLists $ [[1, 7.4, 2.40, 0.599, 0.713]]
    let d2 = M.transpose . fromLists $ [[1, 6.8, 2.25, 0.482, 0.701]]
    let d3 = M.transpose . fromLists $ [[1, 6.8, 2.15, 0.457, 0.734]]
    let d4 = M.transpose . fromLists $ [[1, 7, 2.30, 0.435, 0.764]]

    predict result d1 17.1
    predict result d2 11.6
    predict result d3 5.8
    predict result d4 8.3
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
    
    let result = trainMatrix 0.00001 100000 linear x y w

    putStrLn "학습 결과"
    putStrLn . show $ result
    
{-
128,46,167
166,72,217
-}
    let d1 = M.transpose . fromLists $ [[1,128,46]]
    let d2 = M.transpose . fromLists $ [[1,166,72]]

    predict result d1 167
    predict result d2 217
module Main where

import System.Environment
import Data.Matrix as M
import Data.List   as L
import Text.CSV
import ML.Train
import ML.Example.Regression.Linear

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

    [learningRateString, stepString] <- getArgs

    let learningRate = read learningRateString :: Double
    let step = read stepString :: Int

    (Right rawdata) <- parseCSVFromFile ".\\resource\\data.csv"

    let dataset = L.transpose . map (map (\x -> read x :: Double)) $ rawdata

    let x = fromLists $ (replicate (length . head $ dataset) 1) : init dataset
    let y = fromLists [last dataset]

    let w = fromLists [replicate (nrows x) 0]
    
    let result = trainMatrix learningRate step linear x y w

    putStrLn ""
    putStrLn "학습 결과"
    putStrLn . show $ result
    
{-
167,46,128
217,72,166
-}
    let d1 = M.transpose . fromLists $ [[1,167,46]]
    let d2 = M.transpose . fromLists $ [[1,217,72]]

    predict result d1 128
    predict result d2 166
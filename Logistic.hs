module Logistic where


import ML.Derive
import Data.Matrix

cLog = logBase 10

hypothesis x = 1 / (1 + exp (-x))

cost x y w = (sum . map (\(x, y) -> y * cLog x + (1 - y) * cLog (1 - x)) $ zip hx ys) / len
    where
        hx = map hypothesis . toList $ multStd w x
        ys = toList y
        len = fromIntegral (length ys) :: Double

main = do
    let x = fromLists [[1,1,1,1,1],[1,0,3,0,5],[0,2,0,4,0]]
    let w = fromLists [[0,0,0]]
    let y = fromLists [[1,2,3,4,5]]
    putStrLn "Hello!"
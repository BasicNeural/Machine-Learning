module ML.Example.Classification.Logistic where


import ML.GD
import Data.Matrix

cLog = logBase 10

hypothesis x = 1 / (1 + exp (-x))

logistic x y w = (sum . map (\(xi, yi) -> yi * cLog xi + (1 - yi) * cLog (1 - xi)) $ zip hx ys) / (-len)
    where
        hx = map hypothesis . toList $ multStd w x
        ys = toList y
        len = fromIntegral (length ys) :: Double
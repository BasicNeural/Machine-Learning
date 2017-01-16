module ML.Example.Classification.Logistic where


import ML.GD
import Data.Matrix

hypothesis x = 1 / (1 + exp (-x))

logistic x y w = (sum . map (\(xi, yi) -> yi * log xi + (1 - yi) * log (1 - xi)) $ zip hx ys) / (-len)
    where
        hx = map hypothesis . toList $ multStd w x
        ys = toList y
        len = fromIntegral (length ys) :: Double
{-
logistic x y w = hx / (-len)
    where
        hx = sum . toList $ multStd (fmap (log . hypothesis) $ multStd w x) y
        ys = toList y
        len = fromIntegral (length ys) :: Double
-}
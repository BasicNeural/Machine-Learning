{-
로지스틱 회기 알고리즘
-}
module ML.Example.Classification.Logistic where

import Data.Matrix

hypothesis x = 1 / (1 + exp (-x))

logistic x y w = (sum . map (\(xi, yi) -> yi * log xi + (1 - yi) * log (1 - xi)) $ zip hx ys) / (-len)
    where
        hx = map hypothesis . toList $ w * x
        ys = toList y
        len = fromIntegral (length ys) :: Double
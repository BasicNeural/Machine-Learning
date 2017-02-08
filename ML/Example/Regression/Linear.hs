{-
선형 회기 알고리즘
-}
module ML.Example.Regression.Linear where

import Data.Matrix

linear x y w = (/len) . sum . map (**2) . toList $ w * x - y
    where
        len = fromIntegral (length . toList $ y) :: Double
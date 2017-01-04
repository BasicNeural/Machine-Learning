module ML.Regression.Linear where

import Data.Matrix

linear x y w = (/len) . sum . map (**2) . toList $ multStd w x - y
    where
        len = fromIntegral (length . toList $ y) :: Double
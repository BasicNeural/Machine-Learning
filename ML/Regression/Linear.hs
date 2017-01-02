module ML.Regression.Linear where

import Data.Matrix

linear w x y = scaleMatrix len (multStd (multStd w x - y) $ transpose x)
    where
        len = fromIntegral (length . toList $ y) :: Double
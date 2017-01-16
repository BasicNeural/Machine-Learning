module ML.Softmax where

import Data.Matrix

softmax x = scaleMatrix (1 / (sum . map exp $ toList x)) $ fmap exp x

crossEntorpy x y w = -result
    where
        result = sum . toList $ multStd (fmap log $ softmax (multStd w x)) y
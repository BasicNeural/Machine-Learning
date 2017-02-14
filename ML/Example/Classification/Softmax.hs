module ML.Example.Classification.Softmax where

import Data.Matrix

softmax x = scaleMatrix (1 / (sum . map exp $ toList x)) $ fmap exp x

crossEntropy s l = -result
    where
        result = sum . toList $ (fmap log s) * l

loss x y w = result / len
    where
        len = fromIntegral (ncols x) :: Double
        xs = map (\x -> fromLists $ [x]) . toLists . transpose $ x
        ys = map (\x -> transpose . fromLists $ [x]) . toLists $ y
        ss = map (\x -> softmax $ x*w) xs
        result = sum . map (\(s,l) -> crossEntropy s l) $ zip ss ys
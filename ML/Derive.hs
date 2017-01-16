module ML.Derive where

import Data.Matrix

derive f x = (f (x + delta) - f(x - delta)) / 2.0e-6
    where delta = 1.0e-6

deriveMatrix f w = fromLists [map (\(x, y) -> y / x) . zip deltax . map (\(x, y) -> f (fromLists [y]) - f (fromLists [x])) $ zip (toLists $ ws - deltay) (toLists $ ws + deltay)]
    where
        len = ncols w
        ws = fromLists $ replicate len $ toList w
        delta = 1.0e-6
        deltax = map (\x -> (x + delta) - (x - delta)) $ toList w
        deltay = fromList len len . map (\x -> if x == 1 then delta else 0) $ (toList . identity $ len)

-- 새로운 방법의 미분 함수 만드는중
deriveMatrix' f w = 
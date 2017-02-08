{-
경사 하강을 하는 함수
가장 기본적인 경사 하강 알고리즘을 사용한다
-}
module ML.GD where

import ML.Derive
import Data.Matrix

gd' eps cost old new x y =
    case abs (new - old) > precision of 
        False -> new
        True  -> gd' eps cost new (new - eps * derive (cost x y) new) x y
    where
        precision = 0.0000000001

gd eps cost x y w = gd' eps cost w (w - eps * derive (cost x y) w) x y

gdMatrix' eps cost old new x y =
    case increment > precision of 
        False -> new
        True  -> gdMatrix' eps cost new (new - scaleMatrix eps (deriveMatrix (cost x y) new)) x y
    where
        precision = 0.0000000001
        toNum = sum . map abs . toList
        increment = abs $ toNum new - toNum old

gdMatrix eps cost x y w = gdMatrix' eps cost w (w - scaleMatrix eps (deriveMatrix (cost x y) w)) x y
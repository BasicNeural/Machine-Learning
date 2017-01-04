module ML.GD where

import ML.Derive
import Data.Matrix

gd' cost old new x y =
    case abs (new - old) > precision of 
        True  -> gd' cost new (new - eps * derive (cost x y) new) x y
        False -> new
    where
        precision = 0.0000000001
        eps = 0.0001

gd cost x y w = gd' cost w (w - 0.0001 * derive (cost x y) w) x y

gdMatrix' cost old new x y =
    case increment > precision of 
        True  -> gdMatrix' cost new (new - scaleMatrix eps (deriveMatrix (cost x y) new)) x y
        False -> new
    where
        precision = 0.0000000001
        eps = 0.0001
        toNum = sum . map abs . toList
        increment = abs $ toNum new - toNum old

gdMatrix cost x y w = gdMatrix' cost w (w - scaleMatrix 0.0001 (deriveMatrix (cost x y) w)) x y
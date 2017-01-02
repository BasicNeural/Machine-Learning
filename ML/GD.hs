module GD where

import Data.Matrix

gd' cost old new x y =
    case increment > precision of 
        True  -> gd' cost new (new - scaleMatrix eps (cost new x y)) x y
        False -> new
    where
        precision = 0.0000000001
        eps = 0.0001
        toNum = sum . map abs . toList
        increment = abs $ toNum new - toNum old

gd cost w x y = gd' cost w (w - scaleMatrix 0.0001 (cost w x y)) x y
module ML.Train where

import Data.Matrix
import ML.Derive

train' 0 _ _ new _ _ = new
train' step cost old new x y =
    train' (step - 1) cost new (new - eps * (derive (cost x y) new)) x y
    where
        eps = 0.01

train step cost x y w = train' step cost w (w - 0.01 * (derive (cost x y) w)) x y

trainMatrix' 0 _ _ new _ _ = new
trainMatrix' step cost old new x y =
    trainMatrix' (step - 1) cost new (new - scaleMatrix eps (deriveMatrix (cost x y) new)) x y
    where
        eps = 0.01

trainMatrix step cost x y w = trainMatrix' step cost w (w - scaleMatrix 0.01 (deriveMatrix (cost x y) w)) x y
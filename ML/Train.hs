{-
가중치 학습을 하는 함수
가중치를 n번 학습시키고 싶을 때 사용한다
-}
module ML.Train where

import Data.Matrix
import ML.Derive

train' 0 _ _ new _ _ = new
train' step cost old new x y =
    train' (step - 1) cost new (new - eps * (derive (cost x y) new)) x y
    where
        eps = 0.001

train step cost x y w = train' step cost w (w - 0.001 * (derive (cost x y) w)) x y

trainMatrix' 0 _ _ new _ _ = new
trainMatrix' step cost old new x y =
    trainMatrix' (step - 1) cost new (new - scaleMatrix eps (deriveMatrix (cost x y) new)) x y
    where
        eps = 0.001

trainMatrix step cost x y w = trainMatrix' step cost w (w - scaleMatrix 0.001 (deriveMatrix (cost x y) w)) x y
{-
가중치 학습을 하는 함수
가중치를 n번 학습시키고 싶을 때 사용한다
-}
module ML.Train where

import Data.Matrix
import System.Random
import ML.Derive

train' _ 0 _ _ new _ _ = new
train' eps step cost old new x y =
    train' eps (step - 1) cost new (new - eps * (derive (cost x y) new)) x y

train eps step cost x y w = train' eps step cost w (w - 0.001 * (derive (cost x y) w)) x y

trainMatrix' _ 0 _ _ new _ _ = new
trainMatrix' eps step cost old new x y =
    trainMatrix' eps (step - 1) cost new (new - fmap (*eps) (deriveMatrix (cost x y) new)) x y

trainMatrix eps step cost x y w = trainMatrix' eps step cost w (w - fmap (*eps) (deriveMatrix (cost x y) w)) x y
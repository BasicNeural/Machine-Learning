module ML.NN where

import Data.Matrix
import ML.Derive

data Layer = Layer (Double -> Double) (Matrix Double)

type Network = [Layer]

execute :: Matrix Double -> Layer -> Matrix Double
execute synapse (Layer active body) = fmap active $ body * syn
    where syn = fromLists $ [1] : toLists synapse

executeLayer :: Matrix Double -> Network -> Matrix Double
executeLayer input network = foldl execute input network

sigmoid x = 1 / (1 + exp (-x))

logistic x y = - y * log x - (1 - y) * log x

backpropagation network cost inputs outputs = map (map )
module ML.NN where

import Data.Matrix
import ML.Derive

data Layer = Layer (Double -> Double) (Matrix Double)

instance Show Layer where
    show (Layer _ matrix) = show matrix

type Network = [Layer]

toScalar :: Matrix a -> a
toScalar = head . toList

execute :: Matrix Double -> Layer -> Matrix Double
execute synapse (Layer active body) = fmap active $ body * syn
    where syn = fromLists $ [1] : toLists synapse

executeLayer :: Matrix Double -> Network -> Matrix Double
executeLayer [] input      = input
executeLayer network input = foldl execute input network

sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))


logistic :: Floating a => a -> a -> a->
logistic y y' = - y * log y' - (1 - y) * log y'
--미완성
backpropagation rate network cost inputs outputs = map (\input, output) -> ) $ zip inputs outputs
    where
        delta x y = deriveMatrix (cost y) $ executeLayer x network
        updateNetwork x y = zipWith (\(Layer active synapseX) (Layer _ synapseY) -> Layer active (synapseX - synapseY) ) x y
        backpro [] _ _ = []
        backpro ((Layer active synapse):layers) input delta = newDelta * h : backpro layers input newDelta
            where
                h = executeLayer input $ reverse layers
                newDelta = deriveMatrix (\x -> active . toScalar $ x) $ synapse * h
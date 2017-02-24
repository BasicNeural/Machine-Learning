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
backpropagation rate network cost inputs outputs = 
    where
        hiddenLayer = reverse . init $ network
        (Layer outActive outSynapse) = last network
        deltay x y = deriveMatrix (cost y) y' * deriveMatrix outActive y'        --역전파 에서 가장 처음에 만들어지는 델타 함수
            where y' = executeLayer network x
        updateNetwork x y = zipWith (\(Layer active synapseX) (Layer _ synapseY) -> Layer active (synapseX - fmap (*rate) synapseY) ) x y    --시냅스의 미분 결과로 시냅스를 업데이트
        backpro [] _ _ = []
        backpro ((Layer active synapse):layers) delta = newDelta * h : backpro layers newDelta
            where
                h = map (\x -> executeLayer x $ init . reverse $ layers)
                newDelta = map (\x -> deriveMatrix (\wx -> fmap active wx) $ synapse * x) h
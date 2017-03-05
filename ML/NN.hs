module ML.NN where

import Data.Matrix
import System.Random
import ML.Derive

data Layer = Layer (Double -> Double) (Matrix Double)

instance Show Layer where
    show (Layer _ matrix) = show matrix

instance Num Layer where
    (+) (Layer active lmatrix) (Layer _ rmatrix) = (Layer active (lmatrix + rmatrix))
    (-) (Layer active lmatrix) (Layer _ rmatrix) = (Layer active (lmatrix - rmatrix))
    (*) (Layer active lmatrix) (Layer _ rmatrix) = (Layer active (lmatrix * rmatrix))
    negate = error "ERROR!"
    abs = error "ERROR!"
    signum = error "ERROR!"
    fromInteger = error "ERROR!"

type Network = [Layer]

layerMap f (Layer active synapse) = (Layer active (fmap f synapse))

toScalar :: Matrix a -> a
toScalar = head . toList

addBias :: Num a => Matrix a -> Matrix a
addBias = fromLists . (:) [1] . toLists

execute :: Matrix Double -> Layer -> Matrix Double
execute synapse (Layer active body) = fmap active $ body * (addBias synapse)

executeLayer :: Network -> Matrix Double -> Matrix Double
executeLayer [] input      = input
executeLayer network input = foldl execute input network

sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

logistic :: Floating a => a -> a -> a
logistic y' y = - y * log y' - (1 - y) * log y'

--미완성
backpropagation network cost (x, y) = backpro'
    where
        hiddenLayer = reverse . init $ network
        (Layer outActive outSynapse) = last network
        deltay = derive (cost y) y' * derive outActive y'        --역전파 에서 가장 처음에 만들어지는 델타 함수
            where y' = toScalar $ executeLayer network x
        backpro [] _ = []
        backpro ((Layer active synapse):layers) delta = newDelta * input : backpro layers newDelta
            where
                input = executeLayer (reverse layers) x
                newDelta = delta * fmap (\x -> deriveMatrix active x) input
        backpro' = deltay * input : backpro (reverse hiddenLayer) deltay
            where
                input = executeLayer hiddenLayer x


sdg' eps step cost network dataset = if step == 0
        then return network
        else do
        rand <- randomRIO (0, length dataset - 1)
        let execute = backpropagation network cost (dataset !! fromIntegral rand)
        sdg' eps (step - 1) cost (zipWith (-) network $ map (layerMap (*eps)) execute) dataset

sdg eps step cost network dataset = do
    rand <- randomRIO (0, length dataset - 1)
    let execute = backpropagation network cost (dataset !! fromIntegral rand)
    return $ sdg' eps (step - 1) cost  (zipWith (-) network $ map (layerMap (*eps)) execute) dataset
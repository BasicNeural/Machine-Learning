module ML.NN where

import Data.Matrix
import System.Random
import ML.Derive

data Layer = Layer (Double -> Double) (Matrix Double) (Matrix Double)

instance Show Layer where
    show (Layer _ synapse bias) = "synapse : \n" ++ show synapse ++ "bias : \n" ++ show bias

instance Num Layer where
    (+) (Layer active lmatrix lBias) (Layer _ rmatrix rBias) = (Layer active (lmatrix + rmatrix) (lBias + rBias))
    (-) (Layer active lmatrix lBias) (Layer _ rmatrix rBias) = (Layer active (lmatrix - rmatrix) (lBias - rBias))
    (*) (Layer active lmatrix lBias) (Layer _ rmatrix rBias) = (Layer active (lmatrix * rmatrix) (lBias * rBias))
    negate = error "ERROR!"
    abs = error "ERROR!"
    signum = error "ERROR!"
    fromInteger = error "ERROR!"

type Network = [Layer]

layerMap f (Layer active synapse bias) = (Layer active (fmap f synapse) (fmap f bias))

toScalar :: Matrix a -> a
toScalar = head . toList

execute :: Matrix Double -> Layer -> Matrix Double
execute input (Layer active synapse bias) = fmap active $ synapse * input + bias

executeLayer :: Network -> Matrix Double -> Matrix Double
executeLayer [] input      = input
executeLayer network input = foldl execute input network

sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

logistic :: Floating a => a -> a -> a
logistic d y = - d * log y - (1 - d) * log y

--미완성
backpropagation network (x, y) = reverse backpro'
    where
        hiddenLayer = reverse . init $ network
        (Layer outActive outSynapse _) = last network
        -- 출력층의 델타
        deltaL = input - y
            where input = executeLayer network x
        --
        backpro' = (Layer outActive (deltaL * transpose input) deltaL) : backpro hiddenLayer (deltaL * outSynapse)
            where input = executeLayer (reverse hiddenLayer) x
        --
        backpro [] _ = []
        backpro ((Layer active synapse bias):layers) delta = (Layer active (newDelta * transpose input) newDelta) : backpro layers (newDelta * synapse)
            where input = executeLayer (reverse layers) x
                  newDelta' = (fmap (derive active) $ synapse * input + bias)
                  newDelta = fromLists . map (\x -> [x]) $ zipWith (*) (toList newDelta') (toList delta)

sdg eps step network dataset = if step == 0
        then return network
        else do
        rand <- randomRIO (0, length dataset - 1)
        let execute = backpropagation network (dataset !! fromIntegral rand)
        sdg eps (step - 1) (zipWith (-) network (map (layerMap (*eps)) execute) ) dataset
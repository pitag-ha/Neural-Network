-- in ghci import Systen.Random via Prelude> import System.Random,
-- create generator via Prelude System.Random> gen = mkStdGen <some integer>
-- call constructLayer with gen as first parameter

import System.Random

sigmoidFunction :: Float -> Float
sigmoidFunction x = 1 / (1 + exp (-x))

data Neuron = Essence { weights :: [Float], bias:: Float} deriving (Show)

weightedInput :: Neuron -> [Float] -> Float
weightedInput ess input 
    | (length (weights ess) /= length input) = error "Honk!"
    | otherwise = foldl (\acc pair -> acc + product pair) (bias ess) (zip (weights ess) input) 

type Layer = [Neuron]

constructLayer :: Int -> Int -> Int -> Layer
constructLayer seed number_neurons number_weights = constructLayerRec randList number_neurons number_weights 
    where randList = infRandomList . mkStdGen $ seed

constructLayerRec :: [Float] -> Int -> Int -> Layer
constructLayerRec _ 0 _ = []
constructLayerRec rand number_neurons number_weights = (Essence (tail sub_rand) (head sub_rand) ):(constructLayerRec rest_rand (number_neurons - 1) number_weights)
    where (sub_rand, rest_rand) = splitAt (number_weights + 1) rand 



infRandomList :: StdGen -> [Float]
infRandomList gen = let 
                   (x, newGen) = randomR (-1 :: Float, 1 :: Float) gen
                   in x:(infRandomList newGen)

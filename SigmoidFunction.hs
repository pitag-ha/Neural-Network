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

-- constructLayer :: StdGen -> Int -> Int -> Layer
-- constructLayer gen number_neurons number_weights = [Essence (tail (next index_neuron)) (head (next index_neuron)) | index_neuron <- [0..(number_neurons-1)]]
--     where next index = take (number_weights+1) (drop (index*(number_weights+1)) (infRandomList gen))

constructLayer :: [Float] -> Int -> Int -> Layer
constructLayer' _ 0 _ = []
constructLayer' rand number_neurons number_weights = (Essence (tail sub_rand) (head sub_rand) ):(constructLayer' rest_rand (number_neurons - 1) number_weights)
    where (sub_rand, rest_rand) = splitAt (number_weights + 1) rand 



-- randomList :: StdGen -> Int -> [Float]
-- randomList gen i = case i of
--     0 -> []
--     _ -> let 
--         (x, newGen) = randomR (-1 :: Float, 1 :: Float) gen
--         in x:(randomList newGen (i-1))


infRandomList :: StdGen -> [Float]
infRandomList gen = let 
                   (x, newGen) = randomR (-1 :: Float, 1 :: Float) gen
                   in x:(infRandomList newGen)

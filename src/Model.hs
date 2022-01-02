module Model
    where

import MatrixMult 

etpfET :: [[Double]]
etpfET = [[1.0, 0.0, 1.0, 0.0]
         ,[0.5, 0.0, 0.0, 0.0]
         ,[0.01,0.0, 0.0, 0.0]
         ,[0.0, 0.0, 1.0, 0.0]]

etpfCF :: [[Double]]
etpfCF = [[1.0, 0.0, 1.0, 0.0]
         ,[0.0, 0.0, 0.0, 0.0]
         ,[0.5, 0.0, 0.0, 0.0]
         ,[0.0, 0.0, 1.0, 0.0]]
initial = [1.0, 0.0, 0.0, 0.0]

iterationN :: Num a => Int -> [a] -> [[a]] -> [a]
iterationN n v m = let
    f = last (take n (iterate (`mmult` m) m))
                    in v `vmult` f

iterations :: Num a => Int -> [a] -> [[a]] -> [[a]]
iterations n v m = let fs = take n (iterate (`mmult` m) m) in map (\mi -> v `vmult` mi) fs 

rounded :: Double -> Double
rounded = (/100) . fromIntegral. round . (100.0 *)

total :: [Double] -> Double
total [e,t,p,f] = rounded (e + t)

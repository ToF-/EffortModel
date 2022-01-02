module MatrixMult
    where
import Data.List

mmult :: Num a => [[a]] -> [[a]] -> [[a]]
mmult a b = [ [ sum $ zipWith (*) ar bc | bc <- (transpose b) ] | ar <- a ]

vmult :: Num a => [a] -> [[a]] -> [a]
vmult v m = map (sum . (zipWith (*) v)) m 

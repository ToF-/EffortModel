module Main where

import MatrixMult
import Model


main :: IO ()
main = do
    let s = initial
    putStrLn "#iterations ?"
    n <- read <$> getLine
    let ets = map total $ iterations n initial etpfET
    let cfs =  map total $ iterations n initial etpfCF
    print ets
    print $ sum ets
    print cfs
    print $ sum cfs


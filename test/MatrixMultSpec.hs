module MatrixMultSpec
    where

import Test.Hspec
import MatrixMult

spec :: SpecWith ()
spec = do
    describe "vmult" $ do
        it "multiplies a vector with a matrix" $ do

            let v = [5,3]
            let m = [[1,9]
                    ,[4,8]]
            v `vmult` m `shouldBe` [32,44]


    describe "mmult" $ do
        it "multiplies two matrices" $ do
            let a = [[5,3]
                    ,[2,7]]
            let b = [[1,9]
                    ,[4,8]]
            a `mmult` b `shouldBe`
                    [[17,69]
                    ,[30,74]]

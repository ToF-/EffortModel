module ModelSpec
    where

import Test.Hspec
import Model

spec :: SpecWith ()
spec = do
    describe "edit with test model" $ do
        it "creates half the edition effort for test with limited regression probability" $ do
            etpfET `shouldBe` 
                [[1.0, 0.0, 0.0, 0.1]
                ,[0.5, 0.0, 0.0, 0.0]
                ,[0.01,0.0, 0.0, 0.0]
                ,[0.0, 0.0, 1.0, 0.0]]


    describe "edit with no test model" $ do
        it "creates 0 test effort and .9 of edition effort for regressions" $ do
            etpfCF `shouldBe` 
                [[1.0, 0.0, 0.0, 0.1]
                ,[0.0, 0.0, 0.0, 0.0]
                ,[0.9,0.0, 0.0, 0.0]
                ,[0.0, 0.0, 1.0, 0.0]]



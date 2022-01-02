module ModelSpec
    where

import Test.Hspec
import Model

spec :: SpecWith ()
spec = do
    describe "edit with test model" $ do
        it "edit effort gets 1.0 each iteration, plus regression fixes" $ do
            let r = iterations 3 initial etpfET
            map head r `shouldBe` [1.0, 1.01, 1.02]
        it "total effort include edit effort (which include regression fixes) and test effort which is 50% of edit effort" $ do
            let r = iterations 3 initial etpfET
            map total r `shouldBe` [1.5,1.51,1.52] 

        it "creates half the edition effort for test with limited regression probability" $ do
            etpfET `shouldBe` 
                [[1.0, 0.0, 1.0, 0.0]
                ,[0.5, 0.0, 0.0, 0.0]
                ,[0.01,0.0, 0.0, 0.0]
                ,[0.0, 0.0, 1.0, 0.0]]
        it "after 10 iterations of 1.0 edit effort, cost is around 15" $ do
            let r = rounded $ sum $ map total $ iterations 10 initial etpfET
            r `shouldBe` 15.64
            


    describe "edit with no test model" $ do
        it "creates 0 test effort and .9 of edition effort for regressions" $ do
            etpfCF `shouldBe` 
                [[1.0, 0.0, 1.0, 0.0]
                ,[0.0, 0.0, 0.0, 0.0]
                ,[0.5,0.0, 0.0, 0.0]
                ,[0.0, 0.0, 1.0, 0.0]]

        it "edit effort gets 1.0 each iteration, plus regression fixes" $ do
            let r = iterations 3 initial etpfCF
            map head r `shouldBe` [1.0,1.5,2.0]
        it "total effort include edit effort (which include regression fixes) and no test effort" $ do
            let r = iterations 3 initial etpfCF
            map total r `shouldBe` [1.0,1.5,2.0]
        it "after 10 iterations of 1.0 edit effort, cost is around 64" $ do
            let r = rounded $ sum $ map total $ iterations 10 initial etpfCF
            r `shouldBe` 63.58


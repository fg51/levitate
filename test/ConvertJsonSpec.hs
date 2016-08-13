module ConvertJsonSpec (main, spec) where

import Test.Hspec

import ConvertJson (toJsonString)

main :: IO ()
main = hspec spec


spec :: Spec
spec = describe "test sample head" $ 
    context "when [(0,0,0)] was given" $ do
        it "should return [{\"height\":0,\"time\":0,\"velocity\":0}]" $ do
            toJsonString [(0, 0, 0)] `shouldBe` "[{\"height\":0,\"time\":0,\"velocity\":0}]"

        it "should return [{\"height\":0,\"time\":0,\"velocity\":0}]" $ do
            toJsonString [(0, 0, 0)] `shouldBe` "[{\"height\":0,\"time\":0,\"velocity\":0}]"


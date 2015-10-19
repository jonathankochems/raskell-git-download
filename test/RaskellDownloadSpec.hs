module RaskellDownloadSpec (main, spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = --do
  describe "someFunction" $ 
    it "should work fine" $
      -- TODO: write proper tests 
      True `shouldBe` True

module RaskellDownloadSpec (main, spec) where

import Test.Hspec
import RaskellDownload.Internal

main :: IO ()
main = hspec spec

spec :: Spec
spec = --do
  describe "someFunction" $ 
    it "should work fine" $
      -- TODO: write proper tests 
      rootDir raskellGitDownload `shouldBe` "../"

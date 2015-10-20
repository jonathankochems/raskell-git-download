module RaskellDownloadSpec (main, spec) where

import Test.Hspec
import RaskellDownload.Internal

import System.Directory

main :: IO ()
main = hspec spec

spec :: Spec
spec = --do
  describe "someFunction" $ 
    it "should work fine" $ do
      -- TODO: write proper tests 
      rootDir raskellGitDownload `shouldBe` "../"
      exists <- doesFileExist ".test_output_file" 
      exists `shouldBe` False
      downloadToFile "http://www.bogus.url/" ".test_output_file"
      content <- readFile ".test_output_file" 
      content `shouldBe` ""
      removeFile ".test_output_file"
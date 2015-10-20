module RaskellDownloadSpec (main, spec) where

import Test.Hspec
import RaskellDownload.Internal

import System.Directory

main :: IO ()
main = hspec spec

spec :: Spec
spec = --do
  describe "someFunction" $ 
    it "should work fine" $
      -- TODO: write proper tests 
      rootDir raskellGitDownload `shouldBe` "../"
      doesFileExist ".test_output_file" `shouldBe` False
      downloadToFile "http://www.bogus.url/" ".test_output_file"
      readFile ".test_output_file" `shouldBe` ""
      removeFile ".test_output_file"
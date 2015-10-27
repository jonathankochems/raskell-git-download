module RaskellDownloadSpec (main, spec) where

import Test.Hspec
import RaskellDownload.Internal

import System.Directory

main :: IO ()
main = hspec spec

spec = do helpers
          dataTypes
          gitdownload 

gitdownload :: Spec
gitdownload = 
  describe "RaskellGitDownload" $ 
    it "should download the requested Haskell modules from a GitHub repo" $ do
      let path = ".tmp/Raskell/Test.hs"
      exists <- doesFileExist ".tmp/Raskell/Test.hs" 
      exists `shouldBe` False
      let raskellGitDownloadRepo = Repository{ repository="jonathankochems/raskell-git-download",
                                  			       prefix="src/",
                                      		     branch="develop"
                      						 }
      gitDownload raskellGitDownloadRepo ".tmp/" ["Raskell","Test"]
      content <- readFile path 
      content `shouldBe` "module Raskell.Test where\n"
      removeFile path

helpers :: Spec
helpers = 
  describe "downloadToFile" $ 
    it "should download the content of a URL to a file" $ do
      rootDir raskellGitDownload `shouldBe` "../"
      exists <- doesFileExist ".test_output_file" 
      exists `shouldBe` False
      downloadToFile "https://raw.githubusercontent.com/jonathankochems/raskell-git-download/develop/test/test" ".test_output_file"
      content <- readFile ".test_output_file" 
      content `shouldBe` ""
      removeFile ".test_output_file"

dataTypes :: Spec
dataTypes = do
  describe "Repository" $ 
    it "should describe a git repository, with a branch, and a prefix to the source code modules" $ do
      let raskellGitDownloadRepo = Repository{ repository="jonathankochems/raskell-git-download",
                                  			   prefix="src/",
                                      		   branch="master"
                      						 }
      repository raskellGitDownloadRepo `shouldBe` "jonathankochems/raskell-git-download"
      prefix     raskellGitDownloadRepo `shouldBe` "src/"
      branch     raskellGitDownloadRepo `shouldBe` "master"

  describe "Package" $ 
    it "should describe a Repository, a list of modules, and an installation path" $ do
      let raskellGitDownloadRepo = Repository{ repository="jonathankochems/raskell-git-download",
                                  			   prefix="src/",
                                      		   branch="master"
                      						 }
      rootDir raskellGitDownload `shouldBe` "../"
      modules raskellGitDownload `shouldBe` [["RaskellDownload", "Internal"]]
      packageRepository raskellGitDownload `shouldBe` raskellGitDownloadRepo
      
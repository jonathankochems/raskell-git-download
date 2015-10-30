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
gitdownload = do
    let path = ".tmp/Raskell/Test.hs"
        raskellGitDownloadRepo = Repository{ repository="jonathankochems/raskell-git-download",
                                    			       prefix="src/",
                                        		     branch="develop"
                        						 }
    describe "RaskellGitDownload" $ 
      it "should download the requested Haskell modules from a GitHub repo" $ 
        check_download_is path "module Raskell.Test where\n" $ gitDownload raskellGitDownloadRepo ".tmp/" ["Raskell","Test"]
    describe "RaskellGitDownload" $ 
      it "should download the requested Package from a GitHub repo" $ do
        let package = Package{
                        packageRepository = raskellGitDownloadRepo,
                        rootDir = ".tmp/",
                        modules = [["Raskell", "Test"]]
                      }
        check_download_is path "module Raskell.Test where\n" $ downloadPackage package
  where check_download_is path content m = do  exists <- doesFileExist path
                                               exists `shouldBe` False
                                               m
                                               content' <- readFile path 
                                               content `shouldBe` content'
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
      modules raskellGitDownload `shouldBe` [["RaskellDownload", "Internal"], ["RaskellDownload"]]
      packageRepository raskellGitDownload `shouldBe` raskellGitDownloadRepo
      
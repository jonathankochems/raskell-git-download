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
        raskellGitDownloadRepo = Repository{ owner="jonathankochems",
                                             repository="raskell-git-download",
                                             authToken=Nothing,
                                    			   prefix="src/",
                                             branch="develop",
                                             pathApi=Nothing
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
      let raskellGitDownloadRepo = Repository{ owner="jonathankochems",
                                               repository="raskell-git-download",
                                               authToken=Nothing,
                                               prefix="src/",
                                               branch="develop",
                                               pathApi=Nothing
                                     }
      owner      raskellGitDownloadRepo `shouldBe` "jonathankochems"
      repository raskellGitDownloadRepo `shouldBe` "raskell-git-download"
      prefix     raskellGitDownloadRepo `shouldBe` "src/"
      branch     raskellGitDownloadRepo `shouldBe` "develop"

  describe "Package" $ 
    it "should describe a Repository, a list of modules, and an installation path" $ do
      let raskellGitDownloadRepo = Repository{ owner="jonathankochems",
                                               repository="raskell-git-download",
                                               authToken=Nothing,
                                               prefix="src/",
                                               branch="develop",
                                               pathApi=Nothing
                                     }
      rootDir raskellGitDownload `shouldBe` ""
      modules raskellGitDownload `shouldBe` [["RaskellDownload", "Internal"], ["RaskellDownload"]]
      packageRepository raskellGitDownload `shouldBe` raskellGitDownloadRepo
  describe "PathApi" $ 
    it "should provide URLs and interpretations which give access to the raw contents of repository files" $ do
      githubApiRaw `shouldBe` githubApiV3
      show githubApiRaw `shouldBe` show githubApiV3
      rawUrl githubApiRaw "jonathankochems" "raskell-git-download" "src/Raskell/Test.hs" Nothing Nothing `shouldBe` "https://raw.githubusercontent.com/jonathankochems/raskell-git-download/master/src/Raskell/Test.hs"
      rawUrl githubApiV3 "jonathankochems" "raskell-git-download" "src/Raskell/Test.hs" Nothing Nothing `shouldBe` "https://api.github.com/repos/jonathankochems/raskell-git-download/contents/src/Raskell/Test.hs"
      rawUrl (gogsApiRaw "github.com") "jonathankochems" "raskell-git-download" "src/Raskell/Test.hs" Nothing (Just "TOKEN") `shouldBe` "https://github.com/jonathankochems/raskell-git-download/raw/master/src/Raskell/Test.hs?access_token=TOKEN"
      toRawContents githubApiV3 "" `shouldBe` ""
      toRawContents (gogsApiRaw "github.com") "" `shouldBe` ""
      
-- | Internal module for RaskellDownload
module RaskellDownload.Internal where
import System.Directory
import Raskell.Utils
import Control.Monad (forM_)

downloadToFile url path = 
  do print url
     content <- fetchURL url
     print content
     writeFile path content

-- | Data type to describe repository information. 
--
--   repository contains the github username and the repo name.
--
--   prefix is usually "src/" or something similar.
--
--   branch allows to specify a branch such as master or develop
data Repository = Repository{ repository :: String,
                              prefix     :: String,
                              branch     :: String
                            } deriving (Show, Eq)

-- | The Package type describes all information about a raskell-git-download package.
--  
--   packageRepository describes the repository.
--
--   rootDir specifies in which location in the iOs Raskell filesystem the package will be deployed.
--
--   modules is a list of haskell modules to download.
data Package = Package{ packageRepository :: Repository,
                        rootDir           :: String,
                        modules           :: [[String]]
                      }

gitDownload r targetdir mods = 
  do createDirectoryIfMissing True localDirPath
     downloadToFile url localPath
  where url  = "https://raw.githubusercontent.com/"++repo++"/"++ branchName ++"/" ++ modPrefix ++ modPath
        modPath = dirPath ++ last mods ++ ".hs"
        dirPath = concatMap (let f x = x++"/" in f) (init mods)
        localDirPath = targetdir ++ dirPath
        localPath    = targetdir ++ modPath
        repo         = repository r
        modPrefix    = prefix r
        branchName   = branch r 

-- | installRaskellGitDownload install the installRaskellGitDownload
installRaskellGitDownload = downloadPackage raskellGitDownload

-- | description of raskellGitDownload package
raskellGitDownload = Package{
   packageRepository = Repository{ repository="jonathankochems/raskell-git-download",
                                  prefix="src/",
                                  branch="master"
                      },
   rootDir = "../",
   modules = [["RaskellDownload", "Internal"],
              ["RaskellDownload"]
             ]
}

-- | downloadPackage p downloads a packge.
downloadPackage p = forM_ (modules p) $ gitDownload (packageRepository p) (rootDir p)
module RaskellDownload.Internal where
import System.Directory
import Raskell.Utils
import Control.Monad (forM_)

downloadToFile url path = 
  do print url
     content <- fetchURL url
     print content
     writeFile path content
   
data Repository = Repository{ repository :: String,
                              prefix     :: String,
                              branch     :: String
                            }
  
data Package = Package{ packageRepository :: Repository,
                        rootDir           :: String,
                        modules           :: [[String]]
                      }

gitDownload r targetdir mods = 
  do createDirectoryIfMissing True localDirPath
     downloadToFile url localPath
  where url  = "https://raw.githubusercontent.com/"++repo++"/"++ branchName ++"/" ++ modPrefix ++ modPath
        modPath = dirPath ++ last mods ++ ".hs"
        dirPath = concat $ map (let f x = x++"/" in f) (init mods)
        localDirPath = targetdir ++ dirPath
        localPath    = targetdir ++ modPath
        repo         = repository r
        modPrefix    = prefix r
        branchName   = branch r 
        
bootstrap = downloadPackage raskellGitDownload

raskellGitDownload = Package{
   packageRepository = Repository{ repository="jonathankochems/raskell-git-download",
                                  prefix="src/",
                                  branch="master"
                      },
   rootDir = "../",
   modules = [["RaskellDownload", "Internal"]]
}

downloadPackage p = do forM_ (modules p) $ let f m = gitDownload (packageRepository p) (rootDir p) m in f
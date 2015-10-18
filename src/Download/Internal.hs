import System.Directory
import Raskell.Utils
import Control.Monad (forM_)
-- Adding comment 

escapeSlash = concatMap f
 where f '\\' = "\\\\\\\\"
       f x    = [x]

downloadToFile url path = 
  do print url
     content <- fetchURL url
     print content
     writeFile path $ escapeSlash content
   
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
        dirPath = concat $ map (\x->x++"/") (init mods)
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
   modules = [["Download", "Internal"]]
}

downloadPackage p = do forM_ (modules p) $ \m -> gitDownload (packageRepository p) (rootDir p) m
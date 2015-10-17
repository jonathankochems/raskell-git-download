import System.Directory
import Raskell.Utils

downloadToFile url path = 
  do print url
     content <- fetchURL url
     print content
     writeFile path content
   
data Repository = Repository{ repository :: String,
                              prefix     :: String,
                              branch     :: String
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
        
bootstrap = gitDownload 
              Repository{ repository="jonathankochems/raskell-git-download",
                          prefix="src/",
                          branch="master"
                        }
                        "test/"
                        ["Download", "Internal"]
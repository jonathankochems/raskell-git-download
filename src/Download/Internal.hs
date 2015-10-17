import System.Directory
import Raskell.Utils

downloadToFile url path = 
  do print url
     content <- fetchURL url
     print content
     writeFile path content
     
gitDownload repo mods = 
  do createDirectoryIfMissing True localDirPath
     downloadToFile url localPath
  where url  = "https://raw.githubusercontent.com/"++repo++"/master/src/" ++ modPath
        modPath = dirPath ++ last mods ++ ".hs"
        dirPath = concat $ map (\x->x++"/") (init mods)
        localDirPath = "test/" ++ dirPath
        localPath    = "test/" ++ modPath
        
bootstrap = gitDownload "jonathankochems/raskell-git-download" ["Download", "Internal"]
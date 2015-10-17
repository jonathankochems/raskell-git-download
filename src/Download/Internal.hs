import System.Directory
import Raskell.Utils
--import Text.Parsec


downloadToFile url path = 
  do content <- fetchURL url
     writeFile path content
     
gitDownload mods = 
  do createDirectoryIfMissing True dirPath
     downloadToFile url modPath
  where url  = "https://github.com/"++repo++"/raw/master/" ++ modPath
        repo = "aslatter/parsec"
        --modPath = "Text/Parsec.hs"
        modPath = dirPath ++ last mods ++ ".hs"
        dirPath = concat $ map (\x->x++"/") (init mods)
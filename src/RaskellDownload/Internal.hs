-- | Internal module for RaskellDownload
module RaskellDownload.Internal where
import System.Directory
import Raskell.Utils
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)

downloadToFile url path = 
  do print url
     content <- fetchURL url
     putStrLn $ "received " ++ show (length content) ++ " characters"
     writeFile path content

data PathApi = PathApi { rawUrl :: String -> String -> String -> Maybe String -> Maybe String -> String
                       , toRawContents :: String -> String  } 

instance Show PathApi where
  show x = ""

instance Eq PathApi where
  (==) x y = True


githubApiV3 = PathApi{ rawUrl = let url owner repo path branch token =
                                       intercalate "/" ["https://api.github.com/repos",owner,repo,"contents",path++ parameters]
                                     where parameters | null parameterlist = ""  
                                                      | otherwise          = "? "++concat parameterlist
                                           parameterlist = branchlist ++ authlist 
                                           branchlist    = maybe [] (let f b = ["re f="++b] in f) branch 
                                           authlist      = maybe [] (let f t = ["access_token="++t] in f) token
                                in url,
                        toRawContents = error "not implemented yet"
                     }

githubApiRaw = PathApi{ rawUrl = let url owner repo path branch token =
                                        intercalate "/" ["https://raw.githubusercontent.com",owner,repo,branchname,path]
                                      where branchname = fromMaybe "master" branch 
                                 in url,
                        toRawContents = id
                     }

gogsApiRaw server = PathApi{ rawUrl = let url owner repo path branch token =
                                             intercalate "/" ["https:/",server,"api","v1","repos",owner,repo,"raw",branchname,path++authtoken]
                                           where branchname = fromMaybe "master" branch 
                                                 authtoken  = maybe "" (let f t = "?token="++t in f) token
                                      in url,
                        toRawContents = id
                     }



-- | Data type to describe repository information. 
--
--   repository contains the github username and the repo name.
--
--   prefix is usually "src/" or something similar.
--
--   branch allows to specify a branch such as master or develop
data Repository = Repository{ owner      :: String,
                              repository :: String,
                              authToken  :: Maybe String,
                              prefix     :: String,
                              branch     :: String,
                              pathApi    :: Maybe PathApi
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
  where url     = rawUrl api own repo (modPrefix ++ modPath) (Just branchName) (authToken r) 
        modPath = dirPath ++ last mods ++ ".hs"
        dirPath = concatMap (let f x = x++"/" in f) (init mods)
        localDirPath = targetdir ++ dirPath
        localPath    = targetdir ++ modPath
        repo         = repository r
        own          = owner r
        modPrefix    = prefix r
        branchName   = branch r 
        api          = fromMaybe githubApiRaw $ pathApi r

-- | installRaskellGitDownload install the installRaskellGitDownload
installRaskellGitDownload = downloadPackage raskellGitDownload

-- | description of raskellGitDownload package
raskellGitDownload = Package{
   packageRepository = Repository{ owner="jonathankochems",
                                   repository="raskell-git-download",
                                   authToken=Nothing,
                                   prefix="src/",
                                   branch="develop",
                                   pathApi=Nothing
                      },
   rootDir = "",
   modules = [["RaskellDownload", "Internal"],
              ["RaskellDownload"]
             ]
}

-- | downloadPackage p downloads a packge.
downloadPackage p = forM_ (modules p) $ gitDownload (packageRepository p) (rootDir p)

-- | intercalate from Data.List inlined here for self-containment
intercalate y []     = [] 
intercalate y [x]    = x 
intercalate y (x:xs) = x++y++intercalate y xs

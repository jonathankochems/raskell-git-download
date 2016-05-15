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

-- | Data Type for Path APIs. Default API is 'githubApiRaw'. Other available APIs are 'RaskellDownload.PathApis.gogsApiRaw' and 'RaskellDownload.PathApis.githubApiV3' 
--      in module @RaskellDownload.PathApis@.
--
--   [@rawUrl@] 
--         if we have an @api :: PathApi@ then the expression
--              @downloadUrl = rawUrl api owner repo path branch token@ evaluates to the URL from which the content of 
--              @path@ in @owner@'s repository @repo@ can be downloaded.
--
--   [@toRawContents@] 
--         if @fileContents == fetch downloadUrl@ then
--         @toRawContents api fileContents@ yield the actual file contents from @fileContents@ as a string.
data PathApi = PathApi { rawUrl :: String -> String -> String -> Maybe String -> Maybe String -> String
                       , toRawContents :: String -> String  } 

instance Show PathApi where
  show x = ""

instance Eq PathApi where
  (==) x y = True

-- | Path API for github server (<https://github.com/>)
githubApiRaw = PathApi{ rawUrl = let url owner repo path branch token =
                                        intercalate "/" ["https://raw.githubusercontent.com",owner,repo,branchname,path]
                                      where branchname = fromMaybe "master" branch 
                                 in url,
                        toRawContents = id
                     }

-- | Data type to describe repository information. 
--
--   [@owner@]      @owner@ specifies the github username
--
--   [@repository@] @repository@ contains the repo name.
--
--   [@authToken@]  you can supply an authentication token for a repository that will be
--                  used by the path api to autheticate access to the repository 
--
--   [@prefix@]     @prefix@ is usually "src/" or something similar.
--
--   [@branch@]     @branch@ allows to specify a branch such as master or develop
--   
--   [@pathApi@]    @pathApi@ let's you select an API for repository paths
--                  the default API is for github (@githubApiRaw@) other apis
--                  available in this module are @gogsApiRaw@ and @githubApiV3@
data Repository = Repository{ owner      :: String,
                              repository :: String,
                              authToken  :: Maybe String,
                              prefix     :: String,
                              branch     :: String,
                              pathApi    :: Maybe PathApi
                            } deriving (Show, Eq)

-- | The Package type describes all information about a raskell-git-download package.
--  
--   [@packageRepository@] @packageRepository@ describes the repository.
--
--   [@rootDir@]           @rootDir@ specifies in which location in the iOs Raskell 
--                         filesystem the package will be deployed.
--
--   [@modules@]           @modules@ is a list of haskell modules to download.
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
              ["RaskellDownload", "PathApis"],
              ["RaskellDownload"]
             ]
}

-- | @downloadPackage p@ downloads a packge.
downloadPackage p = forM_ (modules p) $ gitDownload (packageRepository p) (rootDir p)

-- | @intercalate@ from 'Data.List' inlined here for self-containment
intercalate y []     = [] 
intercalate y [x]    = x 
intercalate y (x:xs) = x++y++intercalate y xs

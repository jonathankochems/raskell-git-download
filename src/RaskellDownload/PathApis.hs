-- | Module for RaskellDownload providing a collection of PathApis
module RaskellDownload.PathApis where

import RaskellDownload.Internal(PathApi(..), intercalate)
import Data.Maybe (fromMaybe)

-- | Path API for github server using the github API v3 (<https://developer.github.com/v3/>)
githubApiV3 = PathApi{ rawUrl = let url owner repo path branch token =
                                       intercalate "/" ["https://api.github.com/repos",owner,repo,"contents",path++ parameters]
                                     where parameters | null parameterlist = ""  
                                                      | otherwise          = '?':intercalate "&" parameterlist
                                           parameterlist = branchlist ++ authlist 
                                           branchlist    = maybe [] (let f b = ["ref="++b] in f) branch 
                                           authlist      = maybe [] (let f t = ["access_token="++t] in f) token
                                in url,
                        toRawContents = error "not implemented yet"
                     }

-- | Path API for gogs git server (<https://gogs.io/>)
gogsApiRaw server = PathApi{ rawUrl = let url owner repo path branch token =
                                             intercalate "/" ["https:/",server,"api","v1","repos",owner,repo,"raw",branchname,path++authtoken]
                                           where branchname = fromMaybe "master" branch 
                                                 authtoken  = maybe "" (let f t = "?token="++t in f) token
                                      in url,
                        toRawContents = id
                     }

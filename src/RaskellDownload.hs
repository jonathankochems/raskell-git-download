-- | Simple package to allow haskell module downloads in raskell.
module RaskellDownload ( -- * Packages
                         Package(..),
                         downloadPackage,
                         -- * Repositories 
                        Repository(..),
                        -- * Path Apis 
                         PathApi(..), 
                         githubApiRaw,
                         -- * raskellGitDownload Package
                         raskellGitDownload, 
                         installRaskellGitDownload                       
) where

import RaskellDownload.Internal(downloadPackage, installRaskellGitDownload, raskellGitDownload, Package(..), Repository(..), PathApi(..), githubApiRaw )

{-# LANGUAGE BangPatterns #-}
module Raskell.Utils where

import Network.HTTP.Wget (wget)

fetchURL !url = wget url [] []
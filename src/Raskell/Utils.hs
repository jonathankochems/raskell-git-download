{-# LANGUAGE BangPatterns #-}
module Raskell.Utils where

import Network.HTTP.Conduit

fetchURL !url = do
    request <- parseUrl url
    manager <- newManager tlsManagerSettings
    res <- httpLbs request manager
    return $ responseBody res
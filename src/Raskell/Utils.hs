{-# LANGUAGE BangPatterns #-}
module Raskell.Utils where

import Network.HTTP.Conduit
import Data.ByteString.Lazy.Char8

fetchURL :: String -> IO String
fetchURL !url = do
    request <- parseUrl url
    manager <- newManager tlsManagerSettings
    res <- httpLbs request manager
    return . unpack $ responseBody res
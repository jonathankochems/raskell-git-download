{-# LANGUAGE BangPatterns #-}
module Raskell.Utils where

import qualified Network.HTTP    as HTTP 
import qualified Network.Browser as Browser

fetchURL !url = do 
    (_, rsp) <- Browser.browse $ do
                  Browser.setOutHandler (const $ return ())
                  Browser.setAllowRedirects True -- handle HTTP redirects
                  Browser.request $ HTTP.getRequest url
    return $ HTTP.rspBody rsp
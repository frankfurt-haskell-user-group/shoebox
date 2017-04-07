{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Yesod
import WebBackend
import Data.IORef
import Data.Maybe
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Shoebox.Interface

main :: IO ()
main = do
	shoeDB <- shoeReadDB "frz"
	ref <- newIORef (fromJust shoeDB)
	shoeApp <- toWaiApp (ShoeWeb ref)
	run 3000 $ simpleCors shoeApp

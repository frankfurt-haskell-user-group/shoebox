{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Shoebox.Interface
import Shoebox.Parser
import Shoebox.Data

main :: IO ()
main = do
    p <- loadDB "data/backup/frz.u8" lexDB 
    print (show p)
    return ()
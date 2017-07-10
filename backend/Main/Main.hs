{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Shoebox.Interface
import Shoebox.Parser
import Shoebox.Data
import Shoebox.Util
import Shoebox.QueryRules

import qualified Data.Text as T
import Data.Aeson

import qualified Data.HashMap.Lazy as M 
import qualified Data.Vector as V

data GlobalState = GlobalState {
    gsDataDir :: T.Text, 
    gsDbName :: T.Text,
    gsShoebox :: Shoebox
} 

defaultGS :: IO GlobalState
defaultGS = do
    let (dn, fn) = ("data", "frz")
    sb <- readShoebox dn fn
    let gs = GlobalState dn fn sb 
    return gs

doCommand :: GlobalState -> T.Text -> IO (GlobalState, T.Text)
doCommand gs l = let
    unknownMsg val = (gs, T.concat ["unknown command received: ", (showT val)])  
    cmd = decodeFromText l
    in case cmd of
        Just (Object m) -> case M.lookup "cmd" m of
                            Just (String "current-db") -> return (gs, encodeToText (String . gsDbName $ gs))
                            Just (String "save-db") -> do
                                saveShoebox (gsShoebox gs) (gsDataDir gs) (gsDbName gs) 
                                return (gs, encodeToText (String (T.concat ["saved: ", gsDataDir gs, "/", gsDbName gs])))
                            Just (String "available-dbs") -> do
                                dbs <- listDBs (gsDataDir gs)
                                return (gs, encodeToText . Array . V.fromList $ map String dbs)
                            _ -> return $ unknownMsg cmd
        Just val -> return $ unknownMsg val
        Nothing -> return $ unknownMsg l


main :: IO ()
main = do
    gs <- defaultGS
    let processCommands gs = do
                                l <- readLineConsole
                                (gs', outl) <- doCommand gs l
                                writeLineConsole outl
                                processCommands gs'
    processCommands gs
    return ()
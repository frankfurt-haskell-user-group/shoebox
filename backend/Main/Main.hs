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
        Just (Object m) -> case (M.lookup "cmd" m, M.lookup "para" m) of
                            (Just (String "current-db"), Nothing) -> return (gs, encodeToText (String . gsDbName $ gs))
                            (Just (String "save-db"), Nothing) -> do
                                saveShoebox (gsShoebox gs) (gsDataDir gs) (gsDbName gs) 
                                return (gs, encodeToText (String (T.concat ["saved: ", gsDataDir gs, "/", gsDbName gs])))
                            (Just (String "save-db-as"), Just (String db)) -> do
                                saveShoebox (gsShoebox gs) (gsDataDir gs) db 
                                let gs' = GlobalState (gsDataDir gs) db (gsShoebox gs) 
                                return (gs', encodeToText (String (T.concat ["saved: ", gsDataDir gs', "/", gsDbName gs'])))
                            (Just (String "new-db"), Just (String db)) -> do
                                let gs' = GlobalState (gsDataDir gs) db newShoebox 
                                saveShoebox (gsShoebox gs') (gsDataDir gs') (gsDbName gs') 
                                return (gs', encodeToText (String (T.concat ["created: ", gsDataDir gs', "/", gsDbName gs'])))
                            (Just (String "available-dbs"), Nothing) -> do
                                dbs <- listDBs (gsDataDir gs)
                                let dbs' = Array . V.fromList $ map String dbs
                                let f = (String . gsDbName $ gs)
                                let o = Object $ M.fromList [("dbFile", f), ("availableDBs", dbs')]
                                return (gs, encodeToText o)
                            (Just (String "open-db"), Just (String db)) -> do
                                sb' <- readShoebox (gsDataDir gs) db
                                let gs' = GlobalState (gsDataDir gs) db sb'
                                return (gs', encodeToText (String (T.concat ["opened: ", gsDataDir gs', "/", gsDbName gs'])))
                            (Just (String "delete-db"), Just (String db)) -> do
                                -- deletes current db and opens first db, available
                                -- this should not be called, if there are no additional db's available !
                                deleteDB (gsDataDir gs) db
                                -- now open the new one
                                dbs <- listDBs (gsDataDir gs)
                                let db' = head dbs -- !!
                                sb <- readShoebox (gsDataDir gs) db'
                                return $ (GlobalState (gsDataDir gs) db' sb, encodeToText (String (T.concat ["deleted: ", db, " opened: ", db'])))
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
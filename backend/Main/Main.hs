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

buildMessage msg ob = let
    o = Object $ M.fromList [("msg", msg), ("para", ob)]
    s = encodeToText o
    in T.replace "\n" " " s

resultString msg gs = (String (T.concat [msg, gsDataDir gs, "/", gsDbName gs]))

aDBs gs = do
    dbs <- listDBs (gsDataDir gs)
    let dbs' = Array . V.fromList $ map String dbs
    let f = (String . gsDbName $ gs)
    let o = Object $ M.fromList [("dbFile", f), ("availableDBs", dbs')]
    return $ (buildMessage "dbs" o)


doCommand :: GlobalState -> T.Text -> IO (GlobalState, [T.Text])
doCommand gs l = let
    unknownMsg val = (gs, [(buildMessage "res" (String (T.concat ["unknown command received: ", (showT val)])))])  
    cmd = decodeFromText l
    in case cmd of
        Just (Object m) -> case (M.lookup "cmd" m, M.lookup "para" m) of
                            (Just (String "current-db"), Nothing) -> return (gs, [(buildMessage "cur" (String . gsDbName $ gs))])
                            (Just (String "save-db"), Nothing) -> do
                                saveShoebox (gsShoebox gs) (gsDataDir gs) (gsDbName gs) 
                                return (gs, [(buildMessage "res" (resultString "saved: " gs))])
                            (Just (String "save-db-as"), Just (String db)) -> do
                                saveShoebox (gsShoebox gs) (gsDataDir gs) db 
                                let gs' = GlobalState (gsDataDir gs) db (gsShoebox gs) 
                                m2 <- aDBs gs'
                                let msg = [
                                        (buildMessage "res" (resultString "saved: " gs'))
                                        , m2  
                                        ]
                                return (gs', msg) 

                            (Just (String "new-db"), Just (String db)) -> do
                                let gs' = GlobalState (gsDataDir gs) db newShoebox 
                                saveShoebox (gsShoebox gs') (gsDataDir gs') (gsDbName gs') 
                                m2 <- aDBs gs'
                                return (gs', [
                                    (buildMessage "res" (resultString "created: " gs'))
                                    , m2 
                                    ])

                            (Just (String "available-dbs"), Nothing) -> do
                                m <- aDBs gs
                                return (gs, [m])
                            (Just (String "open-db"), Just (String db)) -> do
                                sb' <- readShoebox (gsDataDir gs) db
                                let gs' = GlobalState (gsDataDir gs) db sb'
                                return (gs', [(buildMessage "res" (resultString "opened: " gs'))])
                            (Just (String "delete-db"), Just (String db)) -> do
                                -- deletes current db and opens first db, available
                                -- this should not be called, if there are no additional db's available !
                                deleteDB (gsDataDir gs) db
                                -- now open the new one
                                dbs <- listDBs (gsDataDir gs)
                                let db' = head dbs -- !!
                                sb <- readShoebox (gsDataDir gs) db'
                                let gs' = GlobalState (gsDataDir gs) db' sb 
                                m2 <- aDBs gs'
                                return $ (gs', 
                                    [
                                        (buildMessage "res" (String (T.concat ["deleted: ", db, " opened: ", db'])))
                                        , m2
                                    ]
                                        )
                            _ -> return $ unknownMsg cmd
        Just val -> return $ unknownMsg val
        Nothing -> return $ unknownMsg l


main :: IO ()
main = do
    gs <- defaultGS
    let processCommands gs = do
                                l <- readLineConsole
                                (gs', outl) <- doCommand gs l
                                mapM writeLineConsole outl
                                processCommands gs'
    processCommands gs
    return ()
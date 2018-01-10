{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Shoebox.Interface
import Shoebox.Parser
import Shoebox.Data
import qualified Shoebox.Command as C
import qualified Shoebox.Response as R
import Shoebox.Util
import Shoebox.TranslationRules

import qualified Data.Text as T
import Data.Maybe

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

buildMessage :: R.Response -> T.Text
buildMessage cmd = encodeToCbor cmd

availableDBs :: GlobalState -> IO T.Text
availableDBs gs = do
    dbs <- listDBs (gsDataDir gs)
    return $ (buildMessage (R.ResFr (R.AvailableDBs dbs)))

doCommand :: GlobalState -> T.Text -> IO (GlobalState, [T.Text])
doCommand gs l = let
    cmd = decodeFromCbor l :: Maybe C.Command
    in case cmd of
        Just (C.CmdFc C.GetCurrentDB) -> return (gs, [(buildMessage (R.ResFr (R.CurrentDBChanged (gsDbName gs))))])

        Just (C.CmdFc C.GetAvailableDBs) -> do
            m <- availableDBs gs
            return (gs, [m, (buildMessage (R.ResFr (R.CurrentDBChanged (gsDbName gs))))])

        Just (C.CmdFc (C.CreateDB db)) -> do
            let gs' = GlobalState (gsDataDir gs) db newShoebox
            saveShoebox (gsShoebox gs') (gsDataDir gs') (gsDbName gs')
            msg <- sequenceA [ return (buildMessage (R.ResFr (R.CreatedDB (gsDbName gs')))) , availableDBs gs']
            return (gs', msg)

        Just (C.CmdFc (C.DeleteDB db)) -> do
            -- deletes current db and opens first db, available
            -- this should not be called, if there are no additional db's available !
            deleteDB (gsDataDir gs) db
            -- now open the new one
            dbs <- listDBs (gsDataDir gs)
            let db' = head dbs -- !!
            sb <- readShoebox (gsDataDir gs) db'
            let gs' = GlobalState (gsDataDir gs) db' sb

            msg <- sequenceA [ return (buildMessage (R.ResFr (R.DeletedDB db))), availableDBs gs' ]
            return (gs', msg)

        Just (C.CmdFc (C.OpenDB db)) -> do
            sb' <- readShoebox (gsDataDir gs) db
            let gs' = GlobalState (gsDataDir gs) db sb'
            return (gs', [buildMessage (R.ResFr (R.OpenedDB (gsDbName gs')))])

        Just (C.CmdFc C.SaveDB) -> do
            saveShoebox (gsShoebox gs) (gsDataDir gs) (gsDbName gs)
            return (gs, [buildMessage (R.ResFr (R.SavedDB (gsDbName gs)))])

        Just (C.CmdFc (C.SaveDBAs db)) -> do
            saveShoebox (gsShoebox gs) (gsDataDir gs) db
            let gs' = GlobalState (gsDataDir gs) db (gsShoebox gs)
            msg <- sequenceA [ return (buildMessage (R.ResFr (R.SavedDB (gsDbName gs')))), availableDBs gs']
            return (gs', msg)

        Just (C.CmdQuery C.DbInfo) -> do
            let sb = gsShoebox gs
            let translations = sbTranslations sb
            let (ShoeboxData dmap)  = sbData sb
            let dataInfo sbdata = (desc, schema) where
                    desc = sbdbDescription sbdata
                    schema = sbdbSchema sbdata
            let dbInfo = fmap dataInfo dmap
            return (gs, [buildMessage (R.ResQuery (R.DbInfo (encodeToText (dbInfo, translations))))])

        Just (C.CmdQuery (C.DbQuery q)) -> do
            let r = translateEntry (gsShoebox gs) (TRN (Right (Just (SbeText q))) [])
            return (gs, [buildMessage (R.ResQuery (R.DbQuery (encodeToText (prettyTRNode r))))])

        Just (C.RunTest q) -> do
            -- check if format "id-..."
            if (T.take 3 q) == "id-"
              then
                let
                  ts = T.split (==' ') q
                  i = head ts
                  t = tail ts
                  r = translateEntry (gsShoebox gs) (TRN (Right (Just (SbeText (t !! 0)))) [])
                in return (gs, [buildMessage (R.TestAnswer ( (T.concat [i, " ", encodeToText (prettyTRNode r)])))])
              else
                return (gs, [buildMessage (R.TestAnswer q)])

        _ -> return $ (gs, [buildMessage R.NoResponse])


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

module Main where

import Prelude
import Control.Monad.Eff.Console (CONSOLE, log) 
import Control.Monad.Eff (Eff)
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import Node.ChildProcess (ChildProcess, CHILD_PROCESS, spawn, defaultSpawnOptions, onExit, stdout)
import Node.Stream (onData)
import Data.Maybe

import GlossingPage (ui)

main :: forall eff. Eff (HalogenEffects (ajax :: AJAX, console::CONSOLE, cp::CHILD_PROCESS )) Unit
main = do
    cp <- spawn "../shoebox/shoe.exe" [] (defaultSpawnOptions {cwd = Just "../shoebox"})
    onData (stdout cp) (\d -> log "received data\n")
    onExit cp (\e -> log "received exit\n") 
    runHalogenAff $ awaitBody >>= runUI ui unit


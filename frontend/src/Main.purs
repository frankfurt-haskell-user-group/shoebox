module Main where

import Prelude
import Control.Monad.Eff.Console (CONSOLE, log) 
import Control.Monad.Eff (Eff)
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import Node.ChildProcess (ChildProcess, CHILD_PROCESS, spawn, exec, defaultSpawnOptions, onExit, stdout, stdin, StdIOBehaviour(..))
import Node.Stream (onData)
import Data.Maybe
import Node.Buffer (BUFFER)
import Node.Stream (writeString, read)
import Node.Encoding (Encoding(..))

import GlossingPage (ui)

main :: forall eff. Eff (HalogenEffects (ajax :: AJAX, console::CONSOLE, cp::CHILD_PROCESS, buffer::BUFFER )) Unit
main = do
    cp <- spawn "backend/shoeB.exe" [] defaultSpawnOptions
    onExit cp (\e -> log "child process exited!\n") 
    runHalogenAff $ awaitBody >>= runUI (ui cp) unit


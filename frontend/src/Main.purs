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
import Node.Platform (Platform(..))
import Node.Process (platform, PROCESS)

import GlossingPage (ui)

main :: forall eff. Eff (HalogenEffects (process::PROCESS, ajax :: AJAX, console::CONSOLE, cp::CHILD_PROCESS, buffer::BUFFER )) Unit
main = do
    let pf = platform 
    cp <- if pf /= Win32 
      then spawn "backend/shoeB" [] defaultSpawnOptions
      else spawn "backend/shoeB.exe" [] defaultSpawnOptions
    onExit cp (\e -> log "child process exited!\n") 
    runHalogenAff $ awaitBody >>= runUI (ui cp) unit


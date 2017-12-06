module CommandDispatcher
where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.StrMap as StrMap
import Data.Argonaut.Core as A

import Control.Monad.Eff.Console (log, CONSOLE) 
import Control.Monad.Eff  (Eff)
import Node.ChildProcess (ChildProcess, CHILD_PROCESS, spawn, exec, defaultSpawnOptions, onExit, stdout, stdin, StdIOBehaviour(..))
import Node.Stream (Writable, onData)
import Control.Monad.Eff.Exception (EXCEPTION)

import ShoeboxChild (shoeboxChild)

data Command = OpenFile String

data CommandDispatcher = CommandDispatcher {
  sendCommand :: forall eff. A.Json -> Eff (console::CONSOLE | eff) Unit
  }

dispatch :: forall eff eff'. Writable ()  eff -> A.Json -> Eff ( console::CONSOLE | eff' )  Unit
dispatch w j = do
  -- we expect a { cmd: String, parameter: obj } Json command, which we need to parse
  let (Tuple cmd para) = A.foldJsonObject (Tuple "" Nothing) (\o ->
                                 case (Tuple (StrMap.lookup "cmd" o) (StrMap.lookup "parameter" o) ) of
                                   Tuple (Just cmd) mbP -> Tuple (A.foldJsonString "" id cmd) mbP
                                   _ -> Tuple "" Nothing
                                ) j
  case cmd of
    "OpenFile" -> handleOpenFile para
    "DeleteFile" -> handleDeleteFile para
    "NewFile" -> handleNewFile para
    "SaveFile" -> handleSaveFile para
    "SaveFileAs" -> handleSaveFileAs para
    _ -> pure unit

    where

      handleOpenFile p = log ("open" <> (show p))
      handleDeleteFile p = log ("del" <> (show p))
      handleNewFile p = log ("new" <> (show p))
      handleSaveFile p = log ("save" <> (show p))
      handleSaveFileAs p = log ("save-as" <> (show p))

receiveData d = pure unit

dispatcher :: forall eff. Eff (console::CONSOLE, cp::CHILD_PROCESS, exception::EXCEPTION | eff) CommandDispatcher
dispatcher = do
  Tuple r w <- shoeboxChild
  let send = dispatch w
--  onData r (\b -> send (A.toJSON b))
  pure (CommandDispatcher { sendCommand: send })

   

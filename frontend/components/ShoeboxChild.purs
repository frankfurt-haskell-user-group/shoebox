module ShoeboxChild where

import Prelude
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff (Eff)
import Node.ChildProcess (ChildProcess, CHILD_PROCESS, spawn, exec, defaultSpawnOptions, onExit, stdout, stdin, StdIOBehaviour(..))
import Node.Stream (writeString, read, Writable, onData, Readable)
import Data.Maybe
import Node.Buffer (BUFFER, Buffer)
import Node.Encoding (Encoding(..))
import Node.Platform (Platform(..))
import Node.Process (platform, PROCESS)
import Data.Tuple (Tuple(..))

type BufferCallback = forall e. Buffer -> Eff e Unit
type WritableResult = Writable () (exception::EXCEPTION, cp::CHILD_PROCESS, console::CONSOLE)
type ReadableResult = Readable () (exception::EXCEPTION, cp::CHILD_PROCESS, console::CONSOLE)

shoeboxChild :: forall eff . Eff (cp::CHILD_PROCESS, console::CONSOLE, exception::EXCEPTION | eff) (Tuple ReadableResult WritableResult)
shoeboxChild = do
    let pf = platform
    cp <- if pf /= Win32
      then spawn "backend/shoeB" [] defaultSpawnOptions
      else spawn "backend/shoeB.exe" [] defaultSpawnOptions
    onExit cp (\e -> log "child process exited!\n")
    pure (Tuple (stdout cp) (stdin cp))


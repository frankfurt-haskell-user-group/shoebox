module DbStore (
  Store,
  makestore,
  dbStore
   ) where

import Prelude

import Control.Monad.Eff (Eff(..))
import Data.Foldable
import Control.Monad.Eff.Ref as R
import Control.Monad.Eff.Unsafe(unsafeCoerceEff)
import Data.StrMap as SM
import Data.Maybe
import Data.Tuple

data Store eff' a = Store {
  read :: forall eff. Eff (ref::R.REF | eff) a,
  write :: forall eff. a -> Eff (ref::R.REF | eff) Unit,
  update :: forall eff. (a -> a) -> Eff (ref::R.REF | eff) Unit,
  subscribe :: forall eff. String -> Eff ( | eff') Unit -> Eff (ref::R.REF | eff) Unit,
  unsubscribe :: forall eff. String -> Eff (ref::R.REF | eff) Unit  }

notifyCo :: forall eff eff'. R.Ref (SM.StrMap (Eff ( | eff') Unit)) -> Eff (ref::R.REF | eff) Unit 
notifyCo r = do
  m <- R.readRef r
  unsafeCoerceEff (sequence_ (SM.values m))
  pure unit

makestore :: forall a eff eff'. a -> Eff ( ref::R.REF | eff) (Store eff' a)
makestore v = do

  dataRef <- R.newRef v
  let s = SM.fromFoldable []
  subRef <- R.newRef s

  let notify r = notifyCo subRef

  let read = R.readRef dataRef
  let write v = do
                  R.writeRef dataRef v
                  (notify subRef)
                  pure unit

  let update f = do
                   (R.modifyRef dataRef f)
                   (notify subRef)
                   pure unit

  let subscribe s f = unsafeCoerceEff $ do
        R.modifyRef subRef (\m -> SM.insert s f m)
        pure unit

  let unsubscribe s = do
        R.modifyRef subRef (\m -> SM.delete s m)
        pure unit

  pure $ Store { read: read, write: write, subscribe: subscribe, update: update, unsubscribe: unsubscribe }

type DbData = { dbs :: Array String, currentDb :: String }

dbStore :: forall eff eff'. DbData -> Eff ( ref::R.REF | eff) Unit (Store eff' DbData)
dbStore d = makestore d

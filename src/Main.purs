module Main where

import Prelude

import Component (Query, component)
import Control.Coroutine (Await)
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Free.Trans (FreeT)
import DOM.HTML.Types (HTMLElement)
import Halogen.Aff as HA

import Halogen.VDom.Driver (runUI)

type AppEffects = ( HA.HalogenEffects (console :: CONSOLE) )
main :: Eff AppEffects Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  log "main"
  run body

rerunUI :: HTMLElement -> Eff AppEffects Unit
rerunUI body = launchAff_ do
  log "rerunUI"
  run body

run ::
  HTMLElement
  -> Aff AppEffects
       { query :: forall a. Query a -> Aff AppEffects a
       , subscribe :: FreeT (Await Void) (Aff AppEffects) Unit
  -> Aff AppEffects Unit
       }
run body = do
  log "run"
  runUI component unit body
